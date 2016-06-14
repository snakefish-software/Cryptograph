package snakefish.crypto
package cipher.historical

import scala.collection.mutable.ArrayBuffer

object Vigenere {
  
  def apply(alphabet: Alphabet, strictMode: Boolean = false) = 
    new Vigenere(alphabet, strictMode)
}

class Vigenere(val alphabet: Alphabet, val strictMode: Boolean = false) {

  @throws(classOf[DataCharNotInAlphabetException])
  def encrypt(key: CharSequence, plaintext: CharSequence): String = 
    crypt(key, plaintext)(addByModulo)
  
  @throws(classOf[DataCharNotInAlphabetException])
  def encrypt(key: Array[Int], plaintext: CharSequence): String = 
    sumKeySeqWithText(key, plaintext, alphabet, strictMode)(addByModulo)

  @throws(classOf[DataCharNotInAlphabetException])
  def decrypt(key: CharSequence, ciphertext: CharSequence): String = 
    crypt(key, ciphertext)(subtractByModulo)
  
  @throws(classOf[DataCharNotInAlphabetException])
  def decrypt(key: Array[Int], ciphertext: CharSequence): String = 
    sumKeySeqWithText(key, ciphertext, alphabet, strictMode)(subtractByModulo)

  private def crypt(
    key: CharSequence,
    data: CharSequence
  )(
    resIndexCalc: (Int, Int, Int) => Int
  ): String = {
    val keyInts = new ArrayBuffer[Int](key.length)
    for (i <- 0 until key.length) {
      val keyChIndex = alphabet.indexOf(key.charAt(i))
      if (keyChIndex >= 0)
        keyInts += keyChIndex
    }
    sumKeySeqWithText(keyInts.toArray, data, alphabet, strictMode)(resIndexCalc)
  }

}
