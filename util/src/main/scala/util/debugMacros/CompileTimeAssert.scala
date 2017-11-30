package org.agdf.util.debugMacros

import scala.language.experimental.macros
import scala.reflect.macros.whitebox._

trait CompileTimeAssert {

  def ensureValidIntLiteral(c: Context)(value: c.Expr[Int], notValidMsg: String, notLiteralMsg: String)(isValid: Int => Boolean): Unit = {
    import c.universe._

    value.tree match {
      case Literal(intConst) =>
        val literalValue = intConst.value.toString.toInt
        if (!isValid(literalValue))
          c.abort(c.enclosingPosition, notValidMsg)
      case _ =>
        c.abort(c.enclosingPosition, notLiteralMsg)
    }
  }

  def ensureValidLongLiteral(c: Context)(value: c.Expr[Long], notValidMsg: String, notLiteralMsg: String)(isValid: Long => Boolean): Unit = {

    import c.universe._

    value.tree match {
      case Literal(longConst) =>
        val literalValue = longConst.value.toString.toLong
        if (!isValid(literalValue))
          c.abort(c.enclosingPosition, notValidMsg)
      case _ =>
        c.abort(c.enclosingPosition, notLiteralMsg)
    }
  }

  def ensureValidFloatLiteral(c: Context)(value: c.Expr[Float], notValidMsg: String, notLiteralMsg: String)(isValid: Float => Boolean): Unit = {

    import c.universe._

    value.tree match {
      case Literal(floatConst) =>
        val literalValue = floatConst.value.toString.toFloat
        if (!isValid(literalValue))
          c.abort(c.enclosingPosition, notValidMsg)
      case _ =>
        c.abort(c.enclosingPosition, notLiteralMsg)
    }
  }

  def ensureValidDoubleLiteral(c: Context)(value: c.Expr[Double], notValidMsg: String, notLiteralMsg: String)(isValid: Double => Boolean): Unit = {

    import c.universe._

    value.tree match {
      case Literal(doubleConst) =>
        val literalValue = doubleConst.value.toString.toDouble
        if (!isValid(literalValue))
          c.abort(c.enclosingPosition, notValidMsg)
      case _ =>
        c.abort(c.enclosingPosition, notLiteralMsg)
    }
  }

  def ensureValidStringLiteral(c: Context)(value: c.Expr[String], notValidMsg: String, notLiteralMsg: String)(isValid: String => Boolean): Unit = {

    import c.universe._

    value.tree match {
      case Literal(stringConst) =>
        val literalValue = stringConst.value.toString
        if (!isValid(literalValue))
          c.abort(c.enclosingPosition, notValidMsg)
      case _ =>
        c.abort(c.enclosingPosition, notLiteralMsg)
    }
  }

  def ensureValidCharLiteral(c: Context)(value: c.Expr[Char], notValidMsg: String, notLiteralMsg: String)(isValid: Char => Boolean): Unit = {

    import c.universe._

    value.tree match {
      case Literal(charConst) =>
        val literalValue = charConst.value.toString.head
        if (!isValid(literalValue))
          c.abort(c.enclosingPosition, notValidMsg)
      case _ =>
        c.abort(c.enclosingPosition, notLiteralMsg)
    }
  }
}

object CompileTimeAssert extends CompileTimeAssert