package net.totietje.ciphers

case class Viginere(ciphertext: String)(implicit alphabet: String = "abcdefghijklmnopqrstuvwxyz") {
  
  /**
    * Does a best guess of the key, given only the expected frequencies of the output.
    *
    * This does a best guess for each substring length between 2 and 10 inclusive, determining which guess is best based
    * on chi squared.
    *
    * If the provided frequencies are correct, and the ciphertext long enough, this will likely get the key.
    */
  def bestGuess(implicit frequencies: Map[Char, Double]): Option[(String, String)] = {
    (2 to 10).flatMap(bestGuess(_)).sortBy {
      case (_, plaintext) => Utils.chiSquared(plaintext)
    }.headOption
  }
  
  /**
    * Does a best guess of what the plaintext is, given the minimum substring length it should look for.
    *
    * First takes a guess at the key length by looking for repeated substrings of a given length, and then
    * uses frequency analysis to determine the key.
    *
    * It may fail if no substrings of the given length are found, returning None.
    *
    * On success, will return Some((key, plaintext))
    */
  def bestGuess(repetitionLength: Int)(implicit frequencies: Map[Char, Double]): Option[(String, String)] = {
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
  def bruteForce(keys: Iterator[String])
    (implicit alphabet: String, frequencies: Map[Char, Double]): Iterator[(String, String, Double)] = {
    
    for (key <- keys) yield {
      val decoded = decrypt(key)
      val distance = Utils.chiSquared(decoded)
      (key, decoded, distance)
    }
  }
  
  /**
    * Given the plaintext and the key length, finds the key
    */
  def findKey(plaintext: String, keyLength: Int): String = {
    ciphertext.take(keyLength).zip(plaintext).map {
      case (a, b) => Caesar.Backwards(a, b)
    }.mkString
  }
  
  /**
    * Decrypts the code with a known key
    */
  def decrypt(key: String): String = {
    Viginere.viginere(ciphertext, key)(Caesar.Backwards)
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
    * Does frequency analysis on all the `5n + k`th elements, where `n` is the key length and k is an arbitrary integer
    * between 0 (inclusive) and n (exclusive). For each index in the key, this finds the optimal character which
    * minimizes chi squared distance from expected frequencies, for all the characters in the text which are caesar
    * shifted by this character. When the key length is known, this will likely find the key, or something very close to
    * it.
    */
  def frequencyAnalysis(keyLength: Int)(implicit frequencies: Map[Char, Double]): (String, String) = {
    val (key, optimisedTranspose) = transpose(keyLength).map(Viginere.optimalCaesarShift).unzip
    
    // Reverse the transpose to extract plaintext
    val untransposedOptions = optimisedTranspose.head.indices.flatMap { i =>
      optimisedTranspose.flatMap { row =>
        row.lift(i)
      }
    }
    
    (key.mkString, untransposedOptions.mkString)
  }
  
  /**
    * Safely transposes the code, returning a sequence of all the strings whose characters are `keyLength` apart.
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
      for (i <- 0 until ciphertext.length by keyLength) yield {
        ciphertext.lift(i + j)
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
    ciphertext.sliding(repetitionsLength).zipWithIndex.toList.groupBy(_._1).map {
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
  def encrypt(plaintext: String, key: String)(implicit alphabet: String): String = {
    viginere(plaintext, key)(Caesar.Forwards)
  }
  
  private def viginere(text: String, key: String)(caesar: Caesar)(implicit alphabet: String): String = {
    // Split the string into key length sized chunks
    (for (chunk <- text.grouped(key.length)) yield {
      /* Pair each chunk with a key letter
       * Eg, with key 'key' and code 'abcdefg':
       *
       * key key k
       * abc def g
       */
      chunk.zip(key).map {
        // Apply rotation to letter
        case (a, b) => caesar(a, b)
      }.mkString
    }).mkString
  }
  
  /**
    * Find the Caesar shift that matches up closes with expected letter frequencies
    */
  private def optimalCaesarShift(str: String)(implicit alphabet: String, frequencies: Map[Char, Double]): (Char, String) = {
    // Apply every possible caesar shift
    val possibilities = for (by <- alphabet) yield {
      (by, Caesar.Backwards(str, by))
    }
    // Then find the one with minimum distance
    possibilities.minBy(x => Utils.chiSquared(x._2))
  }
}
