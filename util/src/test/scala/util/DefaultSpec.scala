package org.agdf.util.test

import scala.language.{implicitConversions, reflectiveCalls, postfixOps}

import org.scalatest._
import prop._

import org.agdf.util._

import org.scalacheck.Gen
import org.scalacheck.Prop.{ collect, classify }
import org.scalacheck.Arbitrary._

class DefaultSpec extends PropSpec with Matchers {
    property("Default value for Boolean") {
      Default.value[Boolean] should be(false)
    }

    property("Default value for Int") {
      Default.value[Int] should be(0)
    }
    
    property("Default value for String") {
      Default.value[String] should be("")
    }

    property("Default value for Option") {
      Default.value[Option[Int]] should be(None)
    }
    
    property("Default value for List") {
      Default.value[List[Int]] should be(List.empty)
    }

    property("Default value for Seq") {
      Default.value[Seq[Int]] should be(Seq.empty)
    }

}