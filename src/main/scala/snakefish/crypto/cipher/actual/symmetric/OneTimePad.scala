package snakefish.crypto
package cipher.actual.symmetric

import OneTimePad._

object OneTimePad {
  
  def apply(alphabet: Alphabet) = new OneTimePad(alphabet)
  
  case class KeyLengthInsuffisientException()
      extends RuntimeException("Key length must be >= data length")
  
  @throws(classOf[KeyLengthInsuffisientException])
  def crypt(key: Array[Byte], data: Array[Byte]): Array[Byte] = {
    if (key.length < data.length)
      throw new KeyLengthInsuffisientException()
    
    xor(key, data)
  }
}

class OneTimePad(val alphabet: Alphabet) {
  
  @throws(classOf[KeyLengthInsuffisientException])
  @throws(classOf[DataCharNotInAlphabetException])
  @throws(classOf[KeyCharNotInAlphabetException])
  def encrypt(key: CharSequence, plaintext: CharSequence): Array[Char] = {
    crypt(key, plaintext)(addByModulo)
  }
  
  @throws(classOf[KeyLengthInsuffisientException])
  @throws(classOf[DataCharNotInAlphabetException])
  @throws(classOf[KeyCharNotInAlphabetException])
  def decrypt(key: CharSequence, ciphertext: CharSequence): Array[Char] = {
    crypt(key, ciphertext)(subtractByModulo)
  }
  
  private def crypt(
    key: CharSequence,
    data: CharSequence
  )(
    resIndexCalc: (Int, Int, Int) => Int
  ): Array[Char] = {
    if (key.length < data.length)
      throw new KeyLengthInsuffisientException()
    
    val result = new Array[Char](data.length)
    for (i <- 0 until data.length) {
      val dataChInd = alphabet.indexOf(data.charAt(i))
      if (dataChInd < 0) {
        erase(result)
        throw new DataCharNotInAlphabetException(i)
      }
      val keyChInd = alphabet.indexOf(key.charAt(i))
      if (keyChInd < 0) {
        erase(result)
        throw new KeyCharNotInAlphabetException(i)
      }
      val resChInd = resIndexCalc(dataChInd, keyChInd, alphabet.length)
      result(i) = alphabet(resChInd)
    }
    result
  }
  
}
