package org.agdf.util.interval

import scala.math.Integral
import org.agdf.util.Equal

trait IntervalLike[+N] {

  def left: N
  def right: N

  def isEmpty: Boolean = false

  def contains[M >: N](value: M)(implicit evM: Integral[M]): Boolean

  def get[M >: N](value: M)(implicit evM: Integral[M]): Option[M] = if (contains(value)) Some(value) else None

  def include[M >: N](other: IntervalLike[M])(implicit evM: Integral[M]): Boolean = other.isEmpty || (this.contains(other.left) && this.contains(other.right))

  def intersect[M >: N](other: IntervalLike[M])(implicit evM: Integral[M]): IntervalLike[M] = {
    if (this.isEmpty || other.isEmpty) Empty
    else {
      val min = evM.max(this.left, other.left)
      val max = evM.min(this.right, other.right)
      IntervalLike.create(min, max)
    }
  }

  def map[M >: N, P](fun: M => P)(implicit evM: Integral[M], evP: Integral[P]): IntervalLike[P] = {
    if (this.isEmpty) Empty
    else IntervalLike.create[P](fun(this.left), fun(this.right))(evP)
  }

  def disjoint[M >: N](other: IntervalLike[M])(implicit evM: Integral[M]): Boolean = intersect(other) == Empty

  def union[M >: N](other: IntervalLike[M])(implicit evM: Integral[M]): IntervalLike[M] = {

    (this, other) match {
      case (Empty, _)           => other
      case (_, Empty)           => this
      case _ if disjoint(other) => Empty
      case _ => {
        val min = evM.min(this.left, other.left)
        val max = evM.max(this.right, other.right)
        IntervalLike.create(min, max)
      }
    }

  }

  def length: Int

}

object IntervalLike {
  def create[T](min: T, max: T)(implicit ev: Integral[T]): IntervalLike[T] = {
    if (min == max) Singleton(min)
    else if (ev.lt(min, max)) Interval(min, max)
    else Empty

  }

  def from[T](left: Singleton[T], right: Singleton[T])(implicit ev: Integral[T]): IntervalLike[T] = {
    create(left.single, right.single)
  }

  implicit def EqualIntervalLike[T: Equal] = new Equal[IntervalLike[T]] {
    val ev = implicitly[Equal[T]]

    override def equal(a1: IntervalLike[T], a2: IntervalLike[T]): Boolean = {
      (a1, a2) match {
        case (Empty, Empty) => true
        case (Singleton(i), Singleton(j)) if ev.equal(i, j) => true
        case (Interval(i1, i2), Interval(j1, j2)) if ev.equal(i1, j1) && ev.equal(i2, j2) => true
        case _ => false
      }
    }
  }

}

//todo : 
// rename Interval => ClosedInterval
// add OpenedInterval
// add Unbounded Interval ?
// todo : un objet appartenant à l'interval et typé (tagé)
// problème, les bornes sont des val, pas incluses dans le type

case class Interval[+N: Integral](left: N, right: N) extends IntervalLike[N] {
  def this(right: N) {
    this(implicitly[Integral[N]].zero, right)
  }

  def this() {
    this(implicitly[Integral[N]].zero, implicitly[Integral[N]].one)
  }

  private[this] val evidence = implicitly[Integral[N]]
  require(evidence.lteq(left, right))

  def leftToSingleton: Singleton[N] = Singleton(left)
  def rightToSingleton: Singleton[N] = Singleton(right)

  def contains[M >: N](value: M)(implicit evM: Integral[M]): Boolean = {
    !isEmpty && (evM.gteq(value, left)) && (evM.lteq(value, right))
  }

  def toStream: Stream[N] = {
    if (isEmpty)
      Stream.empty
    else
      Stream.iterate(left)(evidence.plus(_, evidence.one)).takeWhile(n => evidence.lteq(n, right))
  }

  def toStream[M >: N](f: M => M)(implicit evM: Integral[M]): Stream[M] = {
    if (isEmpty)
      Stream.empty
    else
      Stream.iterate(left.asInstanceOf[M])(f).takeWhile(n => evM.lteq(n, right))
  }

  def length = this.toStream.toSeq.length

}

case class Singleton[+N: Integral](val single: N) extends IntervalLike[N] {
  def this() {
    this(implicitly[Integral[N]].one)
  }

  private[this] val evidence = implicitly[Integral[N]]

  val left, right = single

  def length = 1

  def contains[M >: N](value: M)(implicit evM: Integral[M]): Boolean = {
    !isEmpty && (value == single)
  }

  def toStream: Stream[N] = {
    if (isEmpty)
      Stream.empty
    else
      Stream(single)
  }

}

case object Empty extends IntervalLike[Nothing] {

  override def isEmpty: Boolean = true

  def length = 0

  def contains[M](value: M)(implicit evM: Integral[M]): Boolean = false

  def left: Nothing = ???
  def right: Nothing = ???
}

// TODO : cake pattern pour ce cas là, peut-être utile ?
case class ValueOfInterval[T: Integral](value: T)(implicit interval: IntervalLike[T]) {
  private[this] val evidence = implicitly[Integral[T]]

  require(interval.contains(value))

  def +(other: ValueOfInterval[T]): ValueOfInterval[T] = {
    val nval = evidence.plus(value, other.value)
    if (interval.contains(nval))
      new ValueOfInterval(nval)
    else ???
    // TODO : what to do when overflow ?
  }

  def -(other: ValueOfInterval[T]): ValueOfInterval[T] = {
    val nval = evidence.minus(value, other.value)
    if (interval.contains(nval))
      new ValueOfInterval(nval)
    else ???
    // TODO : what to do when underflow ?
  }

}


