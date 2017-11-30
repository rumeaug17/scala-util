package org.agdf.util.debugMacros

import java.util.Date
import scala.language.experimental.macros
import scala.reflect.macros.whitebox._

object CompileTime {

  def apply(): Date = macro applyImpl

  def applyImpl(c: Context)(): c.Expr[Date] = {
    import c.universe._
    val now = System.currentTimeMillis() // this is executed during compilation
    c.Expr[Date](q"new Date($now)")
  }
}

object Debug {

  def apply(params: Any*): Unit = macro debug_impl
 
   def debug_impl(c: Context)(params: c.Expr[Any]*): c.Expr[Unit] = {
    import c.universe._

    c.warning(c.enclosingPosition, "not using debug macro in production code")

    val trees = params.map { param =>
      param.tree match {
        // Keeping constants as-is
        // The c.universe prefixes aren't necessary, but otherwise Idea keeps importing weird stuff ...
        case c.universe.Literal(c.universe.Constant(const)) => {
          val reified = reify { print(param.splice) }
          reified.tree
        }
        case _ => {
          val paramRep = show(param.tree)
          val paramRepTree = Literal(Constant(paramRep))
          val paramRepExpr = c.Expr[String](paramRepTree)
          val reified = reify { print(paramRepExpr.splice + " = " + param.splice) }
          reified.tree
        }
      }
    }

    // Inserting ", " between trees, and a println at the end.
    val separators = (1 to trees.size-1).map(_ => (reify { print(", ") }).tree) :+ (reify { println() }).tree
    val treesWithSeparators = trees.zip(separators).flatMap(p => List(p._1, p._2))

    c.Expr[Unit](Block(treesWithSeparators.toList, Literal(Constant(()))))
  }
    
}

