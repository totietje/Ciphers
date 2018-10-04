package net.totietje.ciphers

import scala.annotation.tailrec

object Utils {
  /**
    * Greatest common denominator of a list of numbers.
    */
  def gcd(traversable: TraversableOnce[Int]): Int = traversable.reduce(gcd)
  
  /**
    * Greatest common denominator of two numbers.
    */
  @tailrec
  def gcd(a: Int, b: Int): Int = b match {
    case 0 => a
    case _ => gcd(b, a % b)
  }
}
