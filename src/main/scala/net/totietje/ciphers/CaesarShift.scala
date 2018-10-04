package net.totietje.ciphers

/**
  * Represents either a forwards or backwards caesar shift by a given amount
  */
sealed trait CaesarShift {
  def rotate(a: Int, b: Int): Int

  /**
    * 'by' must be lower case.
    */
  def apply(char: Char, by: Char)(implicit language: Language): Char = {
    val byIndex = language.lowerCase.indexOf(by)
    if (byIndex < 0) {
      throw new IllegalArgumentException(s"[by = $by] is invalid lower case char")
    }

    apply(char, byIndex)
  }

  def apply(char: Char, by: Int)(implicit language: Language): Char = {
    val inAlphabet = language.isInAlphabet(char)
    if (!inAlphabet) {
      return char
    }

    val alphabet = if(language.isLowerCase(char)) {
      language.lowerCase
    } else {
      language.upperCase
    }


    val rotated = rotate(alphabet.indexOf(char), by)
    val l = alphabet.length
    val index = ((rotated % l) + l) % l

    alphabet(index)
  }

  def apply(string: String, by: Int)(implicit language: Language): String = for (char <- string) yield {
    apply(char, by)
  }

  /**
    * 'by' must be lower case.
    */
  def apply(string: String, by: Char)(implicit language: Language): String = {
    val byIndex = language.lowerCase.indexOf(by)
    if (byIndex < 0) {
      throw new IllegalArgumentException(s"[by = $by] is invalid lower case char")
    }

    apply(string, byIndex)
  }
}

object CaesarShift {
  case object Forwards extends CaesarShift {
    override def rotate(a: Int, b: Int): Int = a + b
  }

  case object Backwards extends CaesarShift {
    override def rotate(a: Int, b: Int): Int = a - b
  }
}
