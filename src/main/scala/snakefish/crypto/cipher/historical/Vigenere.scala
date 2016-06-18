package snakefish.crypto
package cipher.historical

import scala.collection.mutable.ArrayBuffer

object Vigenere {
  
  def apply(key: CharSequence, alphabet: Alphabet, strictMode: Boolean = false) = 
    new Vigenere(key, alphabet, strictMode)
}

class Vigenere(val key: CharSequence, val alphabet: Alphabet, val strictMode: Boolean = false) {
  
  private val keyIntsList = new ArrayBuffer[Int](key.length)
  for (i <- 0 until key.length) {
    val keyChIndex = alphabet.indexOf(key.charAt(i))
    if (keyChIndex >= 0)
      keyIntsList += keyChIndex
  }
  private val keyInts = keyIntsList.toArray
  
  @throws(classOf[DataCharNotInAlphabetException])
  def encrypt(plaintext: CharSequence): String = 
    sumKeySeqWithText(keyInts, plaintext, alphabet, strictMode)(addByModulo)
  
  @throws(classOf[DataCharNotInAlphabetException])
  def decrypt(ciphertext: CharSequence): String = 
    sumKeySeqWithText(keyInts, ciphertext, alphabet, strictMode)(subtractByModulo)

}
