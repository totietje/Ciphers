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
      'e' -> 0.1202,
  	  't' -> 0.0910,
  	  'a' -> 0.0812,
  	  'o' -> 0.0768,
  	  'i' -> 0.0731,
  	  'n' -> 0.0695,
  	  's' -> 0.0628,
  	  'r' -> 0.0602,
  	  'h' -> 0.0592,
      'd' -> 0.0432,
      'l' -> 0.0398,
      'u' -> 0.0288,
      'c' -> 0.0271,
      'm' -> 0.0261,
      'f' -> 0.0230,
      'y' -> 0.0211,
      'n' -> 0.0209,
      'g' -> 0.0203,
      'p' -> 0.0182,
      'b' -> 0.0149,
      'v' -> 0.0111,
      'k' -> 0.0069,
      'x' -> 0.0017,
      'q' -> 0.0011,
      'j' -> 0.0010,
      'z' -> 0.0007,
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