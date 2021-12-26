package component

import scala.quoted._

import component.BuilderAPI._
import core._

object BuilderAPIMacros {

  class NamedPort(ownerName: Option[String], portName: String) extends Port {
    override def toString = ownerName.fold("")(_ + ".") + portName
  }

  def newPortImpl()(using qctx: Quotes): Expr[Port] = {
    import qctx.reflect._

    def getClassOwner(sym: Symbol = Symbol.spliceOwner): Symbol = {
      if (sym.isClassDef) sym else getClassOwner(sym.owner)
    }

    val portName = Expr(Symbol.spliceOwner.owner.name)
    val thisRef = This(getClassOwner())

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

    '{ new NamedPort($ownerExpr, $portName) }
  }

  def newComponentImpl[A: Type](spec: Expr[Spec[A]])(using qctx: Quotes): Expr[A] = {
    import qctx.reflect._

    val compName = Expr(Symbol.spliceOwner.owner.name)

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
