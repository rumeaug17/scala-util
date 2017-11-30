package org.agdf.util

  import scala.language.postfixOps
  import scala.language.implicitConversions

  import scala.annotation.tailrec


/**
 * TypeClass pour comparaisons typées avec typage statique fort 
 */
trait Equal[-A] {

  def equal(a1: A, a2: A): Boolean = !notEqual(a1, a2)
  def notEqual(a1: A, a2: A): Boolean = !equal(a1, a2)

}

/**
 * Implémentations par défaut de Equal
 */


object Equal {
  implicit object EqualAnyVal extends Equal[AnyVal] {
    override def equal(a1: AnyVal, a2: AnyVal): Boolean = a1 == a2
  }

  implicit object EqualString extends Equal[String] {
    override def equal(a1: String, a2: String): Boolean = a1 == a2
  }

  implicit def EqualOption[T: Equal] = new Equal[Option[T]] {
    override def equal(a1: Option[T], a2: Option[T]): Boolean = (a1, a2) match {
      case (Some(x), Some(y)) => implicitly[Equal[T]].equal(x, y)
      case (None, None)       => true
      case _                  => false
    }
  }

  implicit def EqualTuple2[T1: Equal, T2: Equal] = new Equal[(T1, T2)] {

    type T12 = (T1, T2)
    val evT1 = implicitly[Equal[T1]]
    val evT2 = implicitly[Equal[T2]]

    override def equal(a1: T12, a2: T12): Boolean = evT1.equal(a1._1, a2._1) && evT2.equal(a1._2, a2._2)
  }

  implicit def EqualTuple3[T1: Equal, T2: Equal, T3: Equal] = new Equal[(T1, T2, T3)] {

    type T123 = (T1, T2, T3)
    val evT1 = implicitly[Equal[T1]]
    val evT2 = implicitly[Equal[T2]]
    val evT3 = implicitly[Equal[T3]]

    override def equal(a1: T123, a2: T123): Boolean = evT1.equal(a1._1, a2._1) && evT2.equal(a1._2, a2._2) && evT3.equal(a1._3, a2._3)
  }

  implicit def EqualIterable[T : Equal] = new Equal[Iterable[T]] {
    val ev = implicitly[Equal[T]]
    
    @tailrec override def equal(a1 : Iterable[T], a2 : Iterable[T]) : Boolean = {      
      (a1, a2) match {
        case (Seq(), Seq()) => true
        case (_, Seq()) | (Seq(), _) => false
        case (h1 :: t1, h2 :: t2) if ev.notEqual(h1, h2) => false
        case (h1 :: t1, h2 :: t2) => equal(t1, t2)
      }
      
    }
  }
  
  /*
  implicit def EqualSeq[T : Equal] = new Equal[Seq[T]] {
    val ev = implicitly[Equal[T]]
    
    @tailrec override def equal(a1 : Seq[T], a2 : Seq[T]) : Boolean = {      
      (a1, a2) match {
        case (Seq(), Seq()) => true
        case (x, y) if x.length != y.length => false
        case (h1 :: t1, h2 :: t2) if ev.notEqual(h1, h2) => false
        case (h1 :: t1, h2 :: t2) => equal(t1, t2)
      }
      
    }
  }
  */
  
  /**
   * Extension à Any pour ajouter les opérateurs de comparaison avec typage fort
   */
  implicit class AnyEqualExt[T](val any: T) {

    def ===(a: T)(implicit ev: Equal[T]): Boolean = ev.equal(any, a)

    def =!=(a: T)(implicit ev: Equal[T]): Boolean = ev.notEqual(any, a)
  }

}