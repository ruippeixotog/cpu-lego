package component

import scala.quoted._

import component.BuilderAPI._
import core._

object BuilderAPIMacros {

  class NamedPort(ownerName: Option[String], portName: String) extends Port {
    override def toString = ownerName.fold("")(_ + ".") + portName
  }

  def newPortVec(n: Expr[Int])(using Quotes): Expr[Vector[Port]] =
    '{ Vector.tabulate($n)(idx => ${ newPort(Some('idx)) }) }

  def newPort()(using Quotes): Expr[Port] =
    newPort(None)

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

  def newComponent[A: Type](spec: Expr[Spec[A]])(using qctx: Quotes): Expr[A] = {
    import qctx.reflect._

    def owners = Iterator.iterate(Symbol.spliceOwner)(_.owner)
    lazy val macroOwner = owners.find(!_.flags.is(Flags.Synthetic)).get

    val compName = Expr(macroOwner.name)

    Expr.summon[BuilderEnv] match {
      case None =>
        report.error(s"could not find implicit for ${Type.show[BuilderEnv]}")
        '{ ??? }
      case Some(env) =>
        '{
          val fullCompName = $env.componentName.fold("")(_ + ".") + $compName
          val (res, comp) = buildComponent(Some(fullCompName), $spec)
          $env.add(comp)
          // type cast needed because `buildComponent` is getting incorrectly recognized
          res.asInstanceOf[A]
        }
    }
  }
}
