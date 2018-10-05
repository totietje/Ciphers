package net.totietje.ciphers

import scala.annotation.tailrec

/**
  * Contains info about the alphabet (in case a different one is ever needed) and the frequencies of each character
  * in the alphabet
  */
case class Language(lowerCase: Seq[Char], frequencies: Map[Char, Double]) {
  val upperCase: Seq[Char] = lowerCase.map(_.toUpper)
  val size: Int = lowerCase.size

  def alphabet: Seq[Char] = lowerCase ++ upperCase

  def isInAlphabet(char: Char): Boolean = alphabet.contains(char)
  def isLowerCase(char: Char): Boolean = lowerCase.contains(char)
  def isUpperCase(char: Char): Boolean = upperCase.contains(char)

  /**
    * A measure of how different expected frequencies in a string are compared to actual frequencies. This can be used
    * as a rough, but often accurate, measure of how much the result of a decryption looks like plaintext.
    *
    * Smaller chi squared = more likely to be plaintext.
    */
  def chiSquared(text: String): Double = letterCounts(text).map {
    case (char, observed) =>
      val expected = frequencies(char) * text.length
      val difference = observed - expected
      difference * difference / expected
  }.sum

  /**
    * How many times each character in a string appears.
    *
    * eg, Sos -> Map('S' -> 2, 'o' -> 1)
    */
  def letterCounts(text: String): Map[Char, Int] = {
    text.groupBy(alphabet.indexOf(_) % size).filterKeys(_ != -1).map {
      case (index, str) => (lowerCase(index), str.length)
    }
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
  def keys(length: Int): Iterator[String] = {
    @tailrec
    def keysToTry(acc: Iterator[String], length: Int): Iterator[String] = length match {
      case 0 => acc
      case _ => keysToTry(acc.flatMap(str => lowerCase.map(str + _)), length - 1)
    }
    keysToTry(Iterator(""), length)
  }
}

object Language {
  val English: Language = Language(
    'a' to 'z',
    Map(
      'a' -> 0.08167,
      'b' -> 0.01492,
      'c' -> 0.02782,
      'd' -> 0.04253,
      'e' -> 0.12702,
      'f' -> 0.02228,
      'g' -> 0.02015,
      'h' -> 0.06094,
      'i' -> 0.06966,
      'j' -> 0.00153,
      'k' -> 0.00772,
      'l' -> 0.04025,
      'm' -> 0.02406,
      'n' -> 0.06749,
      'o' -> 0.07507,
      'p' -> 0.01929,
      'q' -> 0.00095,
      'r' -> 0.05987,
      's' -> 0.06327,
      't' -> 0.09056,
      'u' -> 0.02758,
      'v' -> 0.00978,
      'w' -> 0.02360,
      'x' -> 0.00150,
      'y' -> 0.01974,
      'z' -> 0.00074,
    )
  )

  val French: Language = Language(
    'a' to 'z',
    Map(
      'a' -> 0.0808,
      'b' -> 0.0096,
      'c' -> 0.0344,
      'd' -> 0.0408,
      'e' -> 0.1745,
      'f' -> 0.0112,
      'g' -> 0.0118,
      'h' -> 0.0093,
      'i' -> 0.0726,
      'j' -> 0.0030,
      'k' -> 0.0016,
      'l' -> 0.0586,
      'm' -> 0.0278,
      'n' -> 0.0732,
      'o' -> 0.0546,
      'p' -> 0.0298,
      'q' -> 0.0085,
      'r' -> 0.0686,
      's' -> 0.0798,
      't' -> 0.0711,
      'u' -> 0.0559,
      'v' -> 0.0129,
      'w' -> 0.0008,
      'x' -> 0.0043,
      'y' -> 0.0034,
      'z' -> 0.0010,
    )
  )
}