package snakefish.crypto
package cipher.classical

import scala.collection.mutable.ArrayBuffer

object Vigenere {
  
  def apply(key: CharSequence, alphabet: Alphabet, strictMode: Boolean = false) = 
    new Vigenere(key, alphabet, strictMode)
}

class Vigenere(val key: CharSequence, val alphabet: Alphabet, val strictMode: Boolean = false) {
  
  private val keyInts = indicesInAlphabet(key, alphabet)
  
  @throws(classOf[DataCharNotInAlphabetException])
  def encrypt(plaintext: CharSequence): String = 
    sumKeySeqWithText(keyInts, plaintext, alphabet, strictMode)(addByModulo)
  
  @throws(classOf[DataCharNotInAlphabetException])
  def decrypt(ciphertext: CharSequence): String = 
    sumKeySeqWithText(keyInts, ciphertext, alphabet, strictMode)(subtractByModulo)

}
