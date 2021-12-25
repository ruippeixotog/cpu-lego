package component

import scala.quoted._

import component.BuilderDSL._
import core._

object BuilderDSLMacros {

  class NamedPort(ownerName: String, portName: String) extends Port {
    override def toString = s"$ownerName.$portName"
  }

  def newPortImpl()(using qctx: Quotes): Expr[Port] = {
    import qctx.reflect._

    def getClassOwner(sym: Symbol = Symbol.spliceOwner): Symbol = {
      if (sym.isClassDef) sym else getClassOwner(sym.owner)
    }

    val portName = Expr(Symbol.spliceOwner.owner.name)

    val fallbackOwnerExpr = '{ ${ This(getClassOwner()).asExpr }.toString }

    val ownerExpr =
      Expr.summon[BuilderEnv] match {
        case Some(env) => '{ $env.componentName.getOrElse($fallbackOwnerExpr) }
        case None => fallbackOwnerExpr
      }

    '{ new NamedPort($ownerExpr, $portName) }
  }

  def newComponentImpl[A: Type](blockExpr: Expr[BuilderEnv ?=> A])(using qctx: Quotes): Expr[A] = {
    import qctx.reflect._

    val compName = Expr(Symbol.spliceOwner.owner.name)

    Expr.summon[BuilderEnv] match {
      case None =>
        report.error(s"could not find implicit for ${Type.show[BuilderEnv]}")
        '{ ??? }
      case Some(env) =>
        '{
          val fullCompName = $env.componentName.fold("")(_ + ".") + $compName
          val (res, comp) = BuilderDSL.buildComponent(Some(fullCompName), $blockExpr)
          $env.add(comp)
          // type cast needed because `buildComponent` is getting incorrectly recognized
          res.asInstanceOf[A]
        }
    }
  }
}
