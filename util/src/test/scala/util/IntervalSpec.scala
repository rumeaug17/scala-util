package org.agdf.util.test

import scala.language.{ implicitConversions, reflectiveCalls, postfixOps }

import org.scalatest._
import prop._

import org.agdf.util.interval._
import org.agdf.util.Equal
import org.agdf.util.Equal._

import org.scalacheck.Gen
import org.scalacheck.Prop.{ collect, classify }
import org.scalacheck.Arbitrary._

trait CheckIntervalEqual {
  def isEqualToItself(v : IntervalLike[Int]) : Boolean = v === v  
}

trait IntervaGenerators {
  val intervalGen = for {
    i <- Gen.choose(-1000, 1000)
    j <- Gen.choose(0, 10000)
    if j > i
  } yield Interval(i, j)

  val singletonGen = for {
    i <- Gen.choose(-1000, 1000)
  } yield Singleton(i)

    
  val emptyGen = Gen.const(Empty)
  
  val intervalLikeGen = Gen.oneOf(intervalGen, singletonGen, emptyGen)
  
  val unionIntervalGen = for {
    i <- intervalGen
    j <- intervalGen
    if ! i.disjoint(j)
  } yield (i, j)


}

class IntervalPropSpec extends PropSpec with GeneratorDrivenPropertyChecks with Matchers with CheckIntervalEqual with IntervaGenerators {


  property("An interval should be equal to itself") {
    forAll(intervalLikeGen) { i: IntervalLike[Int] =>
      isEqualToItself(i) should be (true)
    }
  }
  
  property("An Empty interval should be empty") {
    forAll { i: Int =>
      Empty.isEmpty should be(true)
      Empty.contains(i) should be(false)
      Empty.get(i) should be(None)
    }
  }

  property("A singleton always including itself") {
    forAll { i: Int =>
      val s = Singleton(i)
      s.include(s) should be(true)
    }
  }

  property("A singleton always contains itself") {
    forAll { i: Int =>
      val s = Singleton(i)
      s.contains(i) should be(true)
    }
  }

  property("A singleton never contains another than itself") {
    forAll { (i: Int, j: Int) =>
      whenever(i != j) {
        val s = Singleton(i)
        s.contains(j) should be(false)
      }
    }
  }

  property("An interval always including itself") {
    forAll(intervalGen) { (i: Interval[Int]) =>
      val si = i.leftToSingleton
      val sj = i.rightToSingleton

      i.include(i) should be(true)
      i.include(si) should be(true)
      i.include(sj) should be(true)

    }
  }

  property("An interval always contains left and right and all values between them") {
    forAll(intervalGen) { (it: Interval[Int]) =>
      (Range.inclusive(it.left, it.right).forall(n => it.contains(n))) should be(true)
    }
  }

  property("An interval always including Empty") {
    forAll(intervalGen) { (it: Interval[Int]) =>
      it.include(Empty) should be(true)
    }
  }
  
  property("An union of two non disjoint interval should include the two original intervals") {
    forAll(unionIntervalGen) { case  (it1 : Interval[Int], it2 : Interval[Int]) =>
      val itRes = it1.union(it2)
      itRes.include(it1) should be (true)
      itRes.include(it2) should be (true)
      
    }
  }

  property("An union with itself shoud be invariant") {
    forAll(intervalGen) { (it: Interval[Int]) =>
      it.union(it) should be(it)
    }
  }
  
  property("An union with empty shoud be invariant") {
    forAll(intervalGen) { (it: Interval[Int]) =>
      it.union(Empty) should be(it)
    }
  }

  property("An intersection with itself shoud be invariant") {
    forAll(intervalGen) { (it: Interval[Int]) =>
      it.intersect(it) should be(it)
    }
  }
  
  property("An intersection with empty shoud be empty") {
    forAll(intervalGen) { (it: Interval[Int]) =>
      it.intersect(Empty) should be(Empty)
    }
  }

}