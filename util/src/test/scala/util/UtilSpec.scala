package org.agdf.util.test

import scala.language.{implicitConversions, reflectiveCalls, postfixOps}

import org.scalatest._
import prop._

import org.agdf.util._
import org.agdf.util.some.{some, none, BooleanExt, StringExt, OptionExt}

import org.scalacheck.Gen
import org.scalacheck.Prop.{ collect, classify }
import org.scalacheck.Arbitrary._

class UtilSpec extends FunSpec with Matchers {
  describe("StringExt") {
    describe("orEmpty") {
      it("should return alternative when string is empty") {
        "".orEmpty("empty") should be ("empty")
      }

      it("should return original string when is not empty") {
        "original".orEmpty("empty") should be ("original")
      }
    }
    
    describe("option") {
      it ("should return None if string is empty") {
        "".option should be (none)
      }
      it ("should return Some if string is not empty") {
        "original".option should be (some("original"))
      }
    }
    
    describe("PositiveInt Tag") {
      it("should be positive integer") {
        PositiveInt(1) should be (1)
      }
      
      it("should be additive group") {
        PositiveInt(1) + PositiveInt(1) should be (2)
      }
    }
  }
}

class UtilPropSpec extends PropSpec with GeneratorDrivenPropertyChecks with Matchers {

  property("A sorted list of Int should be ordered") {
    forAll { l: List[Int] =>
      isOrdered(l.sorted) should be(true)
    }
  }

    property("A reversed sorted list of Int should be reverse ordered") {
    forAll { l: List[Int] =>
      isReverseOrdered(l.sorted.reverse) should be(true)
    }

  }

}
