package snakefish.crypto
package cipher.historical

import scala.collection.mutable.ArrayBuffer

object Vigenere {
  
  def encode(data: CharSequence, key: CharSequence, alphabet: Alphabet): Array[Char] = 
    encode(data, key, alphabet, false)

  @throws(classOf[DataCharNotInAlphabetException])
  def encode(
    data: CharSequence,
    key: CharSequence,
    alphabet: Alphabet,
    strictMode: Boolean
  ): Array[Char] = {
    cryptoFunc(data, key, alphabet, strictMode)(addByModulo)
  }
  
  def encode(data: CharSequence, key: Array[Int], alphabet: Alphabet): Array[Char] = 
    encode(data, key, alphabet, false)
  
  @throws(classOf[DataCharNotInAlphabetException])
  def encode(
    data: CharSequence,
    key: Array[Int],
    alphabet: Alphabet,
    strictMode: Boolean
  ): Array[Char] = {
    sumKeySeqWithText(data, key, alphabet, strictMode)(addByModulo)
  }
  
  def decode(data: CharSequence, key: CharSequence, alphabet: Alphabet): Array[Char] = 
    decode(data, key, alphabet, false)

  @throws(classOf[DataCharNotInAlphabetException])
  def decode(
    data: CharSequence,
    key: CharSequence,
    alphabet: Alphabet,
    strictMode: Boolean
  ): Array[Char] = {
    cryptoFunc(data, key, alphabet, strictMode)(subtractByModulo)
  }
  
  def decode(data: CharSequence, key: Array[Int], alphabet: Alphabet): Array[Char] = 
    decode(data, key, alphabet, false)
  
  @throws(classOf[DataCharNotInAlphabetException])
  def decode(
    data: CharSequence,
    key: Array[Int],
    alphabet: Alphabet,
    strictMode: Boolean
  ): Array[Char] = {
    sumKeySeqWithText(data, key, alphabet, strictMode)(subtractByModulo)
  }

  private def cryptoFunc(
    data: CharSequence,
    key: CharSequence,
    alphabet: Alphabet,
    strictMode: Boolean
  )(
    resIndexCalc: (Int, Int, Int) => Int
  ): Array[Char] = {
    val keyInts = new ArrayBuffer[Int](key.length)
    for (i <- 0 until key.length) {
      val keyChIndex = alphabet.indexOf(key.charAt(i))
      if (keyChIndex >= 0)
        keyInts += keyChIndex
    }
    sumKeySeqWithText(data, keyInts.toArray, alphabet, strictMode)(resIndexCalc)
  }

}
