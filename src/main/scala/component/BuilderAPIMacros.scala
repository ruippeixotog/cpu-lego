package component

import scala.quoted._

import component.BuilderAPI._
import core._

object BuilderAPIMacros {

  class NamedPort(ownerName: Option[String], portName: String) extends Port {
    override def toString = ownerName.fold("")(_ + ".") + portName
  }

  def newBus(n: Expr[Int])(using Quotes): Expr[Bus] =
    registerNewPort('{ Vector.tabulate($n)(idx => ${ newPort(Some('idx)) }) })

  def newPort()(using Quotes): Expr[Port] =
    registerNewPort(newPort(None))

  def newPort(idxExpr: Option[Expr[Int]])(using qctx: Quotes): Expr[Port] = {
    import qctx.reflect._

    def owners = Iterator.iterate(Symbol.spliceOwner)(_.owner)
    lazy val macroOwner = owners.find(!_.flags.is(Flags.Synthetic)).get
    lazy val classOwner = owners.find(_.isClassDef).get

    val portName = Expr(macroOwner.name)
    val portNameWithIdx = idxExpr.fold(portName)(lbl => '{ $portName + "[" + $lbl + "]" })

    val thisRef = This(classOwner)
    val fallbackOwnerExpr =
      if (thisRef.tpe <:< TypeRepr.of[Component]) Some('{ ${ thisRef.asExpr }.toString })
      else None

    val ownerExpr =
      (Expr.summon[BuilderEnv], fallbackOwnerExpr) match {
        case (Some(env), Some(fallback)) => '{ Some($env.componentName.getOrElse($fallback)) }
        case (Some(env), None) => '{ $env.componentName }
        case (None, Some(fallback)) => '{ Some($fallback) }
        case (None, None) => '{ None }
      }

    '{ new NamedPort($ownerExpr, $portNameWithIdx) }
  }

  def newSpec[A: Type](spec: Expr[Spec[A]])(using qctx: Quotes): Expr[A] = {
    import qctx.reflect._

    def owners = Iterator.iterate(Symbol.spliceOwner)(_.owner)
    lazy val macroOwner = owners.find(!_.flags.is(Flags.Synthetic)).get
    val compName = Expr(macroOwner.name)

    def registerArgs(env: Expr[BuilderEnv]): Expr[Unit] = {
      val exprs = macroOwner.paramSymss.flatten.map { sym =>
        val ValDef(name, ttree, _) = sym.tree
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

  def registerPorts[A: Type](env: Expr[BuilderEnv], name: String, expr: Expr[A])(using qctx: Quotes): Expr[Unit] = {
    import qctx.reflect._
    expr match {
      case '{ $port: Port } =>
        '{ $env.register(${ Expr(name) }, $port) }
      case '{ $bus: Bus } =>
        '{ $env.register(${ Expr(name) }, $bus) }
      case '{ $p: (t, u) } =>
        Expr.block(
          List(
            registerPorts(env, s"${name}1", '{ $p._1 }),
            registerPorts(env, s"${name}2", '{ $p._2 })
          ),
          '{}
        )
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

  def registerNewPort[A <: Port | Bus: Type](portExpr: Expr[A])(using qctx: Quotes): Expr[A] = {
    import qctx.reflect._

    def owners = Iterator.iterate(Symbol.spliceOwner)(_.owner)
    val macroOwner = owners.find(!_.flags.is(Flags.Synthetic)).get
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
