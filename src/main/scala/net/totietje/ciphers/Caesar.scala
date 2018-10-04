package net.totietje.ciphers

case class Caesar(ciphertext: String)(implicit language: Language) {
  def decrypt(key: Char): String = {
    CaesarShift.Backwards(ciphertext, key)
  }

  def decrypt(key: Int): String = {
    CaesarShift.Backwards(ciphertext, key)
  }

  /**
    * Runs through all possible caesar shifts and selects the key that minimises chi squared.
    */
  def bestGuess: (Char, String) = {
    // Apply every possible caesar shift
    val possibilities = for (by <- language.lowerCase) yield {
      (by, CaesarShift.Backwards(ciphertext, by))
    }
    // Then find the one with minimum distance
    possibilities.minBy(x => language.chiSquared(x._2))
  }
}

object Caesar {
  def encrypt(plaintext: String, key: Char)(implicit language: Language): String = {
    CaesarShift.Forwards(plaintext, key)
  }

  def encrypt(plaintext: String, key: Int)(implicit language: Language): String = {
    CaesarShift.Forwards(plaintext, key)
  }
}
