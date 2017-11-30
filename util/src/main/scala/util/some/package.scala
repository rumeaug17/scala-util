package org.agdf.util {

package object some {

  import scala.language.{ implicitConversions, reflectiveCalls, postfixOps }

  /**
   * fonction qui construit un objet Some (Option) typé
   */
  def some[A](a: A): scala.Option[A] = Some(a)

  /**
   * fonction qui construit un objet None (Option) typé
   * Ainsi none[Int] <> none[String]
   */
  def none[A]: scala.Option[A] = None

  /**
   * Extension pour Boolean qui ajoute la méthode option
   *
   */
  implicit class BooleanExt(val b: scala.Boolean) extends AnyVal {
    def option[A](e: => A): Option[A] = if (b) some(e) else none[A]
  }

  implicit class StringExt(val str: String) {
  /**
   * Extension pour String, ajoute la méthode orEmpty(s) qui renvoi s à la place de la chaine si celle-ci est vide
   */
    def orEmpty(s: String): String = {
      if (str.isEmpty) s else str
    }
    
  /**
   * Extension pour String, ajoute la méthode option qui renvoi une option sur str (none si chaine vide)
   * option(null) fonctionne déjà

   */
    def option : Option[String] = {
      if (str.isEmpty) none else some(str)
    }
    
  }

  /**
   * Extension pour Option
   */
  implicit class OptionExt[T](val option: scala.Option[T]) {
    /**
     * Ajoute l'opérateur | sur un objet Option comme écriture simplifiée de la méthode getOrElse
     */
    def |[B >: T](a: => B): B = option.getOrElse(a)

    /**
     *
     * Ajoute l'opérateur <*> qui permet d'obtenir Option[A, B] à partir de (Option[A], Option[B]. Renvoit Some ssi les deux sont Some
     *
     */
    def <*>[B](other: scala.Option[B]): scala.Option[(T, B)] = {
      (option, other) match {
        case (None, _) | (_, None) => None
        case (Some(t), Some(b))    => Some(t, b)
      }
    }

    /**
     *
     * opérateur ~ qui remplace la méthode get
     *
     */
    def unary_~ : T = option.get
  }
}
}
