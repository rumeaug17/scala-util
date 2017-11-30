package org.agdf.util.rational

import scala.language.implicitConversions
import scala.math.abs

import org.agdf.util.Equal
import org.agdf.util.math.gcd
import org.agdf.util.some.none
import org.agdf.util.some.some

object RationalImplicits {

  // pourquoi pas dans le companion object directement ?? 
  implicit def intToRational(that: Int) = new Rational(that)
  implicit def doubleToRational(that: Double) = Rational(that)

  /**
   * à utiliser comme ceci : rl"3/2"
   */
  implicit class RationalHelper(private val sc: StringContext) extends AnyVal {
    def rl(args: Any*): Option[Rational] = {

      val s = sc.parts(0).toString

      if (s == "") none
      else
        s.split("/") match {
          case r @ Array(r1, r2) => some(Rational(r1.toInt, r2.toInt))
          case _                 => some(Rational(s.toDouble))
        }
    }
  }

  /**
   * Fractional[Rational]
   * Object implicite pour Fractional (et donc Numeric et Ordering)
   */
  implicit object FractionalImplicit extends Fractional[Rational] {

    def compare(x: Rational, y: Rational): Int = x.compare(y)

    def fromInt(x: Int) = Rational(x)

    def minus(x: Rational, y: Rational) = x - y

    def negate(x: Rational) = -x

    def plus(x: Rational, y: Rational) = x + y

    def times(x: Rational, y: Rational) = x * y

    def div(x: Rational, y: Rational) = x / y

    def toDouble(x: Rational) = x.toDouble

    def toFloat(x: Rational) = x.toDouble.toFloat

    def toInt(x: Rational) = x.toDouble.toInt

    def toLong(x: Rational) = x.toDouble.toLong

  }

  implicit object EqualImplicit extends Equal[Rational] {

    override def equal(a: Rational, b: Rational): Boolean = a.equals(b)
  }
}

class Rational(x: Int, y: Int) extends Ordered[Rational] with Equals {
  require(y != 0, "denominator must be different from zero")

  def this(x: Int) = this(x, 1)
  def this() = this(1, 1)

  private val g = gcd(abs(x), abs(y))

  def numer = x / g
  def denom = y / g

  def compare(that: Rational): Int =
    numer * that.denom - that.numer * denom

  def max(that: Rational) = if (this < that) that else this

  def +(that: Rational) =
    new Rational(
      numer * that.denom + that.numer * denom,
      denom * that.denom)

  def *(that: Rational) =
    new Rational(numer * that.numer, denom * that.denom)

  def /(that: Rational) =
    new Rational(numer * that.denom, denom * that.numer)

  def unary_- = new Rational(-numer, denom)

  def -(that: Rational) = this + -that

  def toDouble: Double = numer.toDouble / denom
  def isInt: Boolean = denom == 1
  def toInt: Option[Int] = if (isInt) Some(numer) else None

  def inverse = new Rational(denom, numer)

  override def toString = if (denom == 1) "" + numer else numer + "/" + denom

  def canEqual(other: Any) = other.isInstanceOf[Rational]

  override def hashCode: Int = 41 * ((41 + numer) + denom)

  override def equals(other: Any) = other match {
    case that: Rational =>
      (that canEqual this) && (numer == that.numer) && (denom == that.denom)
  }

}

object Rational {

  def apply(x: Int, y: Int): Rational = new Rational(x, y)
  def apply(x: Int): Rational = new Rational(x)
  def apply(): Rational = new Rational()

  def apply(d: Double): Rational = approximate(d)

  def unapply(r: Rational): (Int, Int) = (r.numer, r.denom)

  val e = 1e-12

  def approximate(d: Double): Rational =
    if (d.abs < 1) approximate(1000 * d - e, 1000 * d + e) / Rational(1000)
    else if (d.abs < 10) approximate(100 * d - e, 100 * d + e) / Rational(100)
    else if (d.abs < 100) approximate(10 * d - e, 10 * d + e) / Rational(10)
    else approximate(d - e, d + e)

  // approximation d'un reel par le developpement en fraction continue 
  def approximate(d1: Double, d2: Double): Rational = {
    def modf(d: Double): (Double, Double) = {
      val f = d.floor // partie entiere et reste
      (f, d - f)
    }
    import RationalImplicits.intToRational
    (d1, d2) match {
      case (0, _)             => Rational(0)
      case (_, 0)             => Rational(0)
      case (x, y) if (x == y) => throw new Error("unable to calculate approximation for d")
      case (x, y) if (y < 0)  => -approximate(-x, -y)
      case (x, y) => {
        val (xc, xr) = modf(1 / x)
        val (yc, yr) = modf(1 / y)
        if (xc < yc) Rational(1, xc.toInt + 1)
        else if (xc > yc) Rational(1, yc.toInt + 1)
        else 1 / (approximate(xr, yr) + xc.toInt)
      }
    }
  }

}

