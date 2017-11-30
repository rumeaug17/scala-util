package org.agdf.util

import scala.language.{ implicitConversions, reflectiveCalls, postfixOps }
import scala.annotation.tailrec

/**
 * Do class allow to make simple 
 * @param body Code block wich is exexcuted at each iteration
 */
sealed class Do[A](body : => A) {

  /**
   * Continuation condition of the loop
   * The loop is iterated as long as condition is true
   * 
   * @param condition Continuation condition of the loop
   * @return last result of the code-block execution 
   */
  @inline @tailrec def asLongAs(condition : A => Boolean) : A = {
    val result = body
    if (!condition(result)) result else asLongAs(condition)  
  }
  
  /**
   * Termination condition of the loop
   * The loop is iterated until condition is true
   * 
   * @param condition Termination condition of the loop
   * @return last result of the code-block execution 
   */
  @inline @tailrec def until(condition : A => Boolean) : A = {
    val result = body
    if (condition(result)) result else until(condition)  
  }

  @inline @tailrec def nTimes(n : Int) : A = {
    if (n == 0)  body 
    else {
      val result = body ;
      nTimes(n - 1)
    }
  }
}
