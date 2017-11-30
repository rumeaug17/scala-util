package org.agdf.util

import scala.util.Try

/**
 * Nombre Premiers
 */
object Primes {

    private [this]
    lazy val primes : Stream[BigInt] = {
      def from(begin : BigInt, interval : Int) : Stream[BigInt] = {
        begin #:: from(begin + interval, interval)
      }
    
      BigInt(2) #:: from(BigInt(3), 2).filter {
        n => primes.takeWhile(j => j * j <= n).forall(m => m == n || (n % m != 0))
      }
    }

    /**
     * Flux des nombres premiers positifs
     */
    def apply() : Stream[BigInt] = primes

    /**
     * Récupérer le nième nombre premier positif dans l'ordre croissant
     */
    def apply(n : Int) : BigInt = primes(n)
    

    /**
     * Vérifie la primalité d'un nombre
     * Recherche exhaustive
     */
    def isPrime(i : BigInt) : Boolean = {      
      val ai = i.abs
      if (ai <2) false 
      else primes.takeWhile ( _ <= ai ).last == ai 
    }
   
    /**
     * Vérifie la primalité d'un nombre
     * Recherche probabiliste
     */
    def isPPrime(i : BigInt) : Boolean = {
      i.isProbablePrime(40)
    }
}
