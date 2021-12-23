package component

import scala.quoted._

import core._

object BuilderDSLMacros {

  def newPortImpl()(using qctx: Quotes): Expr[Port] = {
    import qctx.reflect._

    def getClassOwner(sym: Symbol = Symbol.spliceOwner): Symbol = {
      if (sym.isClassDef) sym else getClassOwner(sym.owner)
    }

    val portName = Expr(Symbol.spliceOwner.owner.name)
    val thisRef = This(getClassOwner())
    val thisName = Apply(Select.unique(thisRef, "toString"), Nil).asExprOf[String]

    '{
      new Port {
        override def toString = ${ thisName } + "." + ${ portName }
      }
    }
  }
}
