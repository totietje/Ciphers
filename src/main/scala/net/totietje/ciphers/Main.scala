package net.totietje.ciphers

object Main {
  implicit val language: Language = Language.French

  val x: String = """Frqjudwxodwlrqv!
                    |
                    |Li brx kdyh vroyhg wklv, wkhq shukdsv brx zrxog olnh wr hqwhu wkh
                    |
                    |Qdwlrqdo Flskhu Fkdoohqjh?
                    |
                    |
                    |zzz.flskhufkdoohqjh.ruj iru pruh lqirupdwlrq.
                    |
                    |
                    |
                    |Whdpv duh pdgh xs ri dqb vlch dqg iurp dqb bhduv.
                    |Wkh whdp fdswdlq vkrxog hpdlo QFS dw ujvj.fr.xn
                    |
                    |zlwk wkh qdphv ri wkh shrsoh lq brxu whdp iru ixuwkhu lqvwuxfwlrqv.""".stripMargin

  def main(args: Array[String]): Unit = {
    val c = Ciphertext(x)
    println(c.substitution)
  }
}