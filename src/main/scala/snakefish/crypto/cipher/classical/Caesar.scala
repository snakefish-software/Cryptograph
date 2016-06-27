package snakefish.crypto
package cipher.classical

object Caesar {
  
  def apply(key: Int, alphabet: Alphabet, strictMode: Boolean = false) = 
    new Caesar(key, alphabet, strictMode)
}

class Caesar(val key: Int, val alphabet: Alphabet, val strictMode: Boolean = false) {

  @throws(classOf[DataCharNotInAlphabetException])
  def encrypt(plaintext: CharSequence): String = 
    sumKeySeqWithText(_ => key)(plaintext, alphabet, strictMode, addByModulo)

  @throws(classOf[DataCharNotInAlphabetException])
  def decrypt(ciphertext: CharSequence): String = 
    sumKeySeqWithText(_ => key)(ciphertext, alphabet, strictMode, subtractByModulo)

}
