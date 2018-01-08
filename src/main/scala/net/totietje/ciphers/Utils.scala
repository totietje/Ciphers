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
  
  /**
    * Returns an iterator which iterates over all the strings of the given length.
    *
    * eg, for length 2:
    *
    * aa, ab, ac, ad, ae ... ba, bb, bc, bd, be ... zz
    *
    * No guarantee on order. This may be useful for brute forcing when length of key is known and small.
    */
  def keys(length: Int)(implicit alphabet: String): Iterator[String] = {
    @tailrec
    def keysToTry(acc: Iterator[String], length: Int): Iterator[String] = length match {
      case 0 => acc
      case _ => keysToTry(acc.flatMap(str => alphabet.map(str + _)), length - 1)
    }
    keysToTry(Iterator(""), length)
  }
  
  /**
    * A measure of how different expected frequencies in a string are compared to actual frequencies. This can be used
    * as a rough, but often accurate, measure of how much the result of a decryption looks like plaintext.
    *
    * Smaller chi squared = more likely to be plaintext.
    */
  def chiSquared(text: String)(implicit frequencies: Map[Char, Double]): Double = letterCounts(text).map {
    case (char, observed) =>
      val expected = frequencies(char) * text.length
      val difference = observed - expected
      difference * difference / expected
  }.sum
  
  /**
    * How many times each character in a string appears.
    *
    * eg, sos -> Map('s' -> 2, 'o' -> 1)
    */
  def letterCounts(text: String): Map[Char, Int] = text.groupBy(identity).map {
    case (key, str) => (key, str.length)
  }
}
