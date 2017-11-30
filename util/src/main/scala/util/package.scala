
import scala.annotation.tailrec
import scala.language.{ implicitConversions, reflectiveCalls, postfixOps }

import scala.sys.SystemProperties
import java.net.{ Authenticator, PasswordAuthentication }

package org.agdf {

  /**
   * Utility Package
   * Containing differents things you could use to help writing better code
   */
  package object util {

    /**
     * Composition operator
     * Allows to chain functions pipelined fashion
     *
     * @param target Data to be processed by the right-hand function applied by |> operator
     *
     * @example data |> someFunction |> anotherFunction
     */
    implicit def anyWithWorkFlowHelpers[T](target: T) = new {
      def |>[R](fn: T => R) = fn(target)
    }

    /**
     * Authentification proxy pour connexion HTTP
     *
     */
    @inline def authenticate(domain: String, login: String, passwd: String, host: String = "localhost", port: Int = 80): Unit = {
      val props = new SystemProperties()
      props.update("http.proxyHost", host)
      props.update("http.proxyPort", port.toString)
      props.update("http.proxyUser", login)
      props.update("http.proxyPassword", passwd)

      props.update("http.auth.preference", "ntlm")
      props.update("http.auth.ntlm.domain", domain)

      Authenticator.setDefault(new Authenticator() {
        override def getPasswordAuthentication(): PasswordAuthentication = {
          new PasswordAuthentication(login, passwd.toCharArray())
        }
      })

    }

    /**
     * Simple fonction qui ignore son argument mais qui respecte le typage
     */
    def ignore[T >: Null](target: => T): T = null

    /**
     * Vérifie si une liste est triée
     * implicitement dans l'ordre croissant
     *
     * @param items Liste éventuellement triée
     * @param cond Condition de comparaison entre deux éléments de la liste
     */
    def isOrdered[A](items: Iterable[A], cond: (A, A) => Boolean): Boolean =
      items.isEmpty || (items zip items.tail forall (pair => cond(pair._1, pair._2)))

    /**
     * Vérifie si une liste est triée
     * implicitement dans l'ordre croissant
     *
     */
    def isOrdered[A](items: Iterable[A])(implicit ev: Ordering[A]): Boolean =
      items.isEmpty || (items zip items.tail forall (pair => ev.lteq(pair._1, pair._2)))

    /**
     * Vérifie si une liste est triée
     * implicitement dans l'ordre décroissant
     *
     */
    def isReverseOrdered[A](items: Iterable[A])(implicit ev: Ordering[A]): Boolean =
      items.isEmpty || (items zip items.tail forall (pair => ev.gteq(pair._1, pair._2)))

      
    /**
     * Ajoute la fonction or qui permet d'avoir une valeur par défaut en cas d'exception 
     * permet d'écrire des choses comme ça : "1a".toInt or 0
     *
     */
    implicit class OrExt[R](f: => R) {
      def or(r: R): R =
        try f catch {
          case e: Exception => r
        }
    }

    /**
     * Type Tag
     *
     * voir à la place  shapeless.tag.@@
     * en association avec eu.timepit.refined
     */
    @deprecated("utiliser shapeless.tag.@@ à la place", "2015-06-30")
    type Tagged[U] = { type Tag = U }

    @deprecated("utiliser shapeless.tag.@@ à la place", "2015-06-30")
    type @@[T, U] = T with Tagged[U]

    @deprecated("utiliser shapeless.tag.@@ à la place", "2015-06-30")
    object Tag {
      @inline def apply[T, U](t: T): T @@ U = t.asInstanceOf[T @@ U]
    }

    // exemple d'utilisation du Tag
    // très incomplet, la moindre opération fait perdre le tag, car il s'agit d'un nouvel objet
    @deprecated("utiliser refined \"String @@ MaxSize[Witness.`32`.T]\" à la place", "2015-07-01")
    sealed trait String32
    object String32 {
      def apply(value: String): String @@ String32 = {
        require(value.length <= 32)
        Tag(value)
      }

      def unapply(s: String @@ String32): Option[String] = Some(s)

    }

    @deprecated("utiliser refined \"Int @@ Interval[_0, _10]\" à la place", "2015-07-01")
    sealed trait IntFrom0To10
    object IntFrom0To10 {
      def apply(value: Int): Int @@ IntFrom0To10 = {
        require(value >= 0 && value <= 10)
        Tag(value)
      }

      def unapply(s: Int @@ IntFrom0To10): Option[Int] = Some(s)

    }

    // autre exemple d'utilisation du Tag
    // avec macros pour vérifier à la compilation
    sealed trait PositiveInt
    object PositiveInt {

      import scala.language.experimental.macros
      import scala.reflect.macros.whitebox._
      import org.agdf.util.debugMacros.CompileTimeAssert

      // utilisation d'une macro pour vérifier à la compilation que l'entier est bien >= 0
      // et qu'il s'agit d'un littéral
      // mais ça ne règle rien
      private[this] object PositiveIntMacro extends CompileTimeAssert {
        def apply(c: Context)(value: c.Expr[Int]): c.Expr[Int @@ PositiveInt] = {
          val notValidMsg = "Not A valid Integer Value"
          val notLiteralMsg = "Not a Litteral Value"

          import c.universe._

          ensureValidIntLiteral(c)(value, notValidMsg, notLiteralMsg) { i => i >= 0 }
          reify { value.splice.asInstanceOf[Int @@ PositiveInt] }
        }
      }

      def apply(value: Int): Int @@ PositiveInt = macro PositiveIntMacro.apply

      def unapply(p: Int @@ PositiveInt): Option[Int] = some.some(p)
    }

  }

}
