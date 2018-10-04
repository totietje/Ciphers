package net.totietje.ciphers

case class Substitution(ciphertext: String)(implicit language: Language) {
  def decrypt(key: Map[Char, Char]): String = Substitution.encrypt(ciphertext, key.map(_.swap))
}

object Substitution {
  def encrypt(plaintext: String, key: Map[Char, Char])(implicit language: Language): String = plaintext.map {
    c: Char => if (key.isDefinedAt(c)) {
      key(c)
    } else if (key.isDefinedAt(c.toLower)) {
      key(c.toLower).toUpper
    } else {
      c
    }
  }
}
