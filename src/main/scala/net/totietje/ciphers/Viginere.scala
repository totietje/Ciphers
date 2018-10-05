package net.totietje.ciphers

case class Viginere(ciphertext: String)(implicit language: Language) {
  /**
    * Lower case version of ciphertext, which does not include any of the characters outside the given alphabet.
    */
  val clean: String = ciphertext.filter(language.isInAlphabet).toLowerCase

  /**
    * Does a best guess of the key, given only the expected frequencies of the output.
    *
    * This does a best guess for each substring length between 2 and 10 inclusive, determining which guess is best based
    * on chi squared.
    *
    * If the provided frequencies are correct, and the ciphertext long enough, this will likely get the key 
    */
  def bestGuess: Option[(String, String)] = {
    (2 to 10).flatMap(bestGuess(_)).sortBy {
      case (_, plaintext) => language.chiSquared(plaintext)
    }.headOption
  }

  /**
    * Does a best guess of what the plaintext is.
    *
    * First takes a guess at the key length by looking for repeated substrings of the given length, and then
    * uses frequency analysis to determine the key.
    *
    * Long text will usually have more, longer repeats but may also have more coincidental repeats so the
    * repetitionLength should be increased.
    *
    * It may fail if no repeated substrings of the given length are found, returning None.
    *
    * On success, will return Some((key, plaintext))
    */
  def bestGuess(repetitionLength: Int): Option[(String, String)] = {
    keyLengthGuess(repetitionLength).map(frequencyAnalysis)
  }

  /**
    * Applies each key in the iterator to the ciphertext and calculates the chi squared value of the ciphertext.
    *
    * If the correct key was in the iterator, the plaintext will likely be the string with the minimum chi squared.
    *
    * Yields an iterator over tuple (key, decrypted, chi squared of decrypted)
    *
    * Care should be taken not to load all the iterator into memory at once (so no calling .toList), as this will
    * likely result in running out of memory.
    */
  def bruteForce(keys: Iterator[String]): Iterator[(String, String, Double)] = {
    for (key <- keys) yield {
      val decoded = decrypt(key)
      val distance = language.chiSquared(decoded)
      (key, decoded, distance)
    }
  }

  /**
    * Given the plaintext and the key length, finds the key
    */
  def findKey(plaintext: String, keyLength: Int): String = {
    clean.take(keyLength).zip(plaintext).map {
      case (a, b) => CaesarShift.Backwards(a, b)
    }.mkString
  }

  /**
    * Decrypts the code with a known key
    */
  def decrypt(key: String): String = {
    Viginere.viginere(ciphertext, key)(CaesarShift.Backwards)
  }

  /**
    * Takes a guess at the key length by looking at the gaps between repetitions of a certain length, and taking their
    * gcd
    */
  def keyLengthGuess(repetitionsLength: Int): Option[Int] = gaps(repetitionsLength).keys match {
    case iterable if iterable.isEmpty => None // No gaps detected
    case gaps => Some(gaps.reduce(Utils.gcd))
  }

  /**
    * If `n` is the key length, then for every `k` between 0 (inclusive) and n (exclusive) this takes all the `an + k`th
    * elements - that is, all the elements which have been caesar shifted by the same letter. Frequency analysis is then
    * performed on the generated strings to find the associated character which minimises chi squared. When the key
    * length is known, this will likely find the key, or something very close to it.
    */
  def frequencyAnalysis(keyLength: Int): (String, String) = {
    val (keySeq, _) = transpose(keyLength).map(Caesar(_).bestGuess).unzip
    val key = keySeq.mkString
    (key, decrypt(key))
  }

  /**
    * Safely transposes the cleaned ciphertext, returning a sequence of all the strings whose characters are `keyLength` apart.
    *
    * Eg, abcdefghijklmnopqr with keyLength 4:
    *
    * abcd
    * efgh
    * ijkl
    * mnop
    * qr
    *
    * [aeimq, bfjnr, cgko, dhlp]
    */
  private def transpose(keyLength: Int): Seq[String] = {
    val transposeOption = for (j <- 0 until keyLength) yield {
      for (i <- 0 until clean.length by keyLength) yield {
        clean.lift(i + j)
      }
    }
    transposeOption.map(_.flatten.mkString)
  }

  /**
    * Counts how many times substrings are repeated in the code
    *
    * @param repetitionsLength
    *                          The length of the substrings to be detected
    * @return
    *         A map giving how many times a gap size appears
    */
  def gaps(repetitionsLength: Int): Map[Int, Int] = {
    // For each n letters in a row, get their indices and group same combinations together
    clean.sliding(repetitionsLength).zipWithIndex.toList.groupBy(_._1).map {
      // Now ignore everything except for the indices of repetitions
      case (_, list) => list.map(_._2)
    }.flatMap { indices =>
      // Convert the indices into the gaps between them
      indices.sliding(2).collect {
        case Seq(a, b) => b - a
      }
    }.groupBy(identity).map { // Group by size of gap
      case (gap, gaps) => (gap, gaps.size) // Count instances of each gap size
    }
  }
}

object Viginere {
  /**
    * Encrypts the given plaintext with the given key
    */
  def encrypt(plaintext: String, key: String)(implicit language: Language): String = {
    viginere(plaintext, key)(CaesarShift.Forwards)
  }

  private def viginere(text: String, key: String)(caesar: CaesarShift)(implicit language: Language): String = {
    text.foldLeft(("", 0)) {
      case ((current, len), c) => if (language.isInAlphabet(c)) {
         (current + caesar(c, key(len)), (len + 1) % key.length)
      } else {
        (current + c, len)
      }
    }._1
  }
}