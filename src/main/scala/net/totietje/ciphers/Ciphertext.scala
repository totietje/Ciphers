package net.totietje.ciphers

case class Ciphertext(ciphertext: String)(implicit val language: Language) {
  def viginere: Viginere = Viginere(ciphertext)

  def caesar: Caesar = Caesar(ciphertext)

  def substitution: Substitution = Substitution(ciphertext)
}