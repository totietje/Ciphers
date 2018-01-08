package net.totietje.ciphers

sealed trait Caesar {
  def apply(char: Char, by: Char)(implicit alphabet: String): Char = {
    val rotated = rotate(alphabet.indexOf(char), alphabet.indexOf(by))
    val l = alphabet.length
    val index = ((rotated % l) + l) % l
  
    alphabet(index)
  }
  
  def rotate(a: Int, b: Int): Int
  
  def apply(string: String, by: Char)(implicit alphabet: String): String = for (char <- string) yield {
    apply(char, by)
  }
}

object Caesar {
  case object Forwards extends Caesar {
    override def rotate(a: Int, b: Int): Int = a + b
  }
  
  case object Backwards extends Caesar {
    override def rotate(a: Int, b: Int): Int = a - b
  }
}
