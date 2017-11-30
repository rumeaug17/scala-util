package org.agdf.util.test

import scala.language.{ implicitConversions, reflectiveCalls, postfixOps }

import org.scalatest._
import prop.GeneratorDrivenPropertyChecks

import org.agdf.util.Equal
import org.agdf.util.Equal._
import org.agdf.util.some._

import org.scalacheck.Gen

trait CheckEqual {
  // on passe par là pour éviter la surcharge === dans PropSpec
  def isEqual(x: Int, y: Int) = x === y

  def isEqual(x: String, y: String) = x === y

  def isEqual(x: List[Boolean], y: List[Boolean]) = x === y

  def isNotEqual(x: List[Int], y: List[Int]) = x =!= y
}

class EqualSpec extends PropSpec with Matchers with CheckEqual {
  property("some(4) is not equal to some(1)") {
    (some(4) =!= some(1)) should be(true)
  }

}

class EqualPropSpec extends PropSpec with GeneratorDrivenPropertyChecks with Matchers with CheckEqual {

  property("Any Int must be  equal to himself") {
    forAll { x: Int =>
      isEqual(x, x) should be(true)
    }
  }

  property("Any List of Bool must be  equal to himself") {
    forAll { x: List[Boolean] =>
      isEqual(x, x) should be(true)
    }
  }

  property("Some List of Int, when append one element, are not equal to original") {
    forAll { (x: List[Int], i : Int) =>
      val y = x :+ i

      isNotEqual(x, y) should be(true)
    }
  }

  property("Some List of Int, when insert one element, are not equal to original") {
    forAll { (x: List[Int], i : Int) =>
      val y = i :: x

      isNotEqual(x, y) should be(true)
    }
  }
}