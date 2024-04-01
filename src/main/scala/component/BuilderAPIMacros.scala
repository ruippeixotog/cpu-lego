package component

import scala.quoted._

import component.BuilderAPI._
import core._

import BuilderAPIMacros._

object BuilderAPIMacros {

  class NamedPort(owner: Option[String], name: String) extends Port {
    override def toString = owner.fold("")(_ + ".") + name
  }

  def newBus(n: Expr[Int])(using Quotes): Expr[Bus] =
    BuilderAPIMacros().newBus(n)

  def newPort()(using Quotes): Expr[Port] =
    BuilderAPIMacros().newPort()

  def newSpec[A: Type](spec: Expr[Spec[A]])(using Quotes): Expr[A] =
    BuilderAPIMacros().newSpec(spec)
}

class BuilderAPIMacros()(using qctx: Quotes) {
  import qctx.reflect._

  def newBus(n: Expr[Int]): Expr[Bus] =
    registerNewPort('{ Vector.tabulate($n)(idx => ${ newPort(Some('idx)) }) })

  def newPort(): Expr[Port] =
    registerNewPort(newPort(None))

  def newPort(idxExpr: Option[Expr[Int]]): Expr[Port] = {
    val portName = Expr(macroOwner.name)
    val portNameWithIdx = idxExpr.fold(portName)(lbl => '{ $portName + "[" + $lbl + "]" })

    val ownerExpr =
      Expr.summon[BuilderEnv] match {
        case Some(env) => '{ $env.componentName }
        case None => '{ None }
      }

    '{ new NamedPort($ownerExpr, $portNameWithIdx) }
  }

  def newSpec[A: Type](spec: Expr[Spec[A]]): Expr[A] = {
    val compName = Expr(macroOwner.name)

    def registerArgs(env: Expr[BuilderEnv]): Expr[Unit] = {
      val exprs = macroOwner.paramSymss.flatten.map { sym =>
        val ValDef(name, ttree, _) = sym.tree: @unchecked
        ttree.tpe.asType match {
          case '[t] => registerPorts(env, name, Ref(sym).asExprOf[t])
        }
      }
      Expr.block(exprs, '{})
    }

    val env = Expr.summon[BuilderEnv].getOrElse {
      report.errorAndAbort(s"could not find implicit for ${Type.show[BuilderEnv]}")
    }

    '{
      val (res, comp) = buildComponent(
        Some($compName), {
          val innerEnv = summon[BuilderEnv]
          val innerRes = $spec
          ${ registerArgs('innerEnv) }
          ${ registerPorts('innerEnv, "out", 'innerRes) }
          innerRes
        }
      )
      $env.add($compName, comp)
      res
    }
  }

  // ---------

  private def owners: Iterator[Symbol] =
    Iterator.iterate(Symbol.spliceOwner)(_.owner)

  private lazy val macroOwner: Symbol =
    owners.find(!_.flags.is(Flags.Synthetic)).get

  def registerPorts[A: Type](env: Expr[BuilderEnv], name: String, expr: Expr[A]): Expr[Unit] = {
    expr match {
      case '{ $port: Port } =>
        '{ $env.register(${ Expr(name) }, $port) }
      case '{ $bus: Bus } =>
        '{ $env.register(${ Expr(name) }, $bus) }
      case '{ $p: Tuple } =>
        def registerTuple[B <: Tuple: Type](exprB: Expr[B], idx: Int): List[Expr[Unit]] = {
          exprB match {
            case '{ $p: EmptyTuple } => Nil
            case '{ $p: (t *: u) } =>
              registerPorts(env, s"${name}${idx}", '{ $p.head }) :: registerTuple('{ $p.tail }, idx + 1)
          }
        }
        Expr.block(registerTuple(p, 1), '{})
      case '{ $obj: t } =>
        Expr.block(
          TypeRepr.of[t].typeSymbol.caseFields.map { sym =>
            val (field, ttree) = sym.tree match {
              case ValDef(field, ttree, _) => (field, ttree)
              case DefDef(field, _, ttree, _) => (field, ttree)
            }
            ttree.tpe.asType match {
              case '[t] => registerPorts(env, name + "_" + field, Select(obj.asTerm, sym).asExprOf[t])
              case _ => '{}
            }
          },
          '{}
        )
    }
  }

  def registerNewPort[A <: Port | Bus: Type](portExpr: Expr[A]): Expr[A] = {
    val portName = Expr(macroOwner.name)

    Expr.summon[BuilderEnv] match {
      case Some(env) =>
        '{
          val p = $portExpr
          $env.register($portName, p)
          p
        }
      case _ =>
        portExpr
    }
  }
}
