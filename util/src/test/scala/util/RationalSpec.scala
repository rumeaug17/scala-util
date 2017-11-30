package org.agdf.util.test

import scala.language.{ implicitConversions, reflectiveCalls, postfixOps }

import org.scalatest._
import prop._

import org.agdf.util.rational._
import org.agdf.util.isOrdered
import org.agdf.util.some._

import RationalImplicits._

import org.scalacheck.Gen
import org.scalacheck.Prop.{ collect, classify }
import org.scalacheck.Arbitrary._

class RationalSpec extends PropSpec with Matchers {
  property("1/2 should be equal to 1/2") {
    Rational(1, 2).toString should be("1/2")
  }

  property("7/14 should be reduce to 1/2") {
    Rational(7, 14).toString should be("1/2")
  }

  property("2/2 should be an integer unit value") {
    Rational(2, 2) should be(Rational(1))
    Rational(2, 2).isInt should be(true)
  }

  property("1/2 + 1/2 should be 1") {
    Rational(1, 2) + Rational(1, 2) should be(Rational(1))
  }

  property("1/2 - 1/3 should be 1/6") {
    Rational(1, 2) - Rational(1, 3) should be(Rational(1, 6))
  }

  property("5/2 should be appromitate to 2.5d") {
    Rational(5, 2).toDouble should equal(2.5d)
  }

  property("rounding 2.333333333333333333 should be 7/3") {
    Rational(2.333333333333333333) should be(Rational(7, 3))
  }

  property("Max of 1/2 and 1/3 must be 1/2") {
    (Rational(1, 2) max Rational(1, 3)) should be(Rational(1, 2))
  }

  property("1/2 div 1/3 should be 3/2") {
    Rational(1, 2) / Rational(1, 3) should be(Rational(3, 2))
  }

  property("1/2 * 1/3 should be 1/6") {
    Rational(1, 2) * Rational(1, 3) should be(Rational(1, 6))
  }

  property("1/2 * 3 should be 3/2") {
    Rational(1, 2) * 3 should be(Rational(3, 2))
  }

  property("3/2 should be the inverse of 2/3") {
    Rational(3, 2).inverse should be(Rational(2, 3))
  }

  property("-3/2 should be -3/2") {
    -Rational(3, 2) should be(Rational(-3, 2))
  }

  property("1/2 should be < 5/8") {
    assert(Rational(1, 2) < Rational(5, 2))
  }

  property("1/2 - 1/2 should be 0") {
    Rational(1, 2) - Rational(1, 2) should be (Rational(0))
  }
  
  property("rl\"1/2\" should be real 1/2") {
    ~rl"1/2" should be (Rational(1, 2))
  }
  
  property("rl\"2\" should be real 2") {
    ~rl"2" should be (Rational(2))
  }

  property("rl\"2.5\" should be real 5/2") {
    ~rl"2.5" should be (Rational(5, 2))
  }
  
  property("rl\"\" should be none") {
    rl"" should be (none)
  }

}

class RationalPropSpec extends PropSpec with GeneratorDrivenPropertyChecks with Matchers {

  val rationalGen = for {
    i <- Gen.choose(-10000, 10000)
    j <- Gen.choose(1, 10000)
  } yield Rational(i, j)

  val rationalListGen = Gen.containerOf[List, Rational](rationalGen)

  property("A sorted list of Rational should be ordered") {
    forAll(rationalListGen) { l: List[Rational] =>
      isOrdered(l.sorted) should be(true)
    }

  }

}
