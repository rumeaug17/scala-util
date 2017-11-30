package org.agdf.util

trait State[S, A] {
  val run: S => (S, A)
  def apply(s: S): (S, A) = run(s)
  def eval(s: S): A = run(s)._2
  def exec(s: S): S = run(s)._1

  def map[B](f: A => B): State[S, B] =
    State { s: S =>
      val (s1, a) = run(s)
      (s1, f(a))
    }

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State { s: S =>
      val (s1, a) = run(s)
      f(a)(s1)
    }

}

object State {
  def apply[S, A](f: S => (S, A)): State[S, A] = new State[S, A] {
    final val run = f
  }

  def init[S, A](a: A) = State { s: S => (s, a) }
  def update[S, A](f: S => S): State[S, Unit] = State { s: S => (f(s), ()) }
  def get[S] : State[S, S] = State { s => (s, s) }
  def gets[S, A](f: S => A): State[S, A] = State { s: S => (s, f(s)) }

  def put[S](s : S) : State[S, Unit] = State { _ => (s, ()) }
}
