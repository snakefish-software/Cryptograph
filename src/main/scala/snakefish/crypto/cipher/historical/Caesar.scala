package snakefish.crypto
package cipher.historical

object Caesar {
  
  def apply(alphabet: Alphabet, strictMode: Boolean = false) = 
    new Caesar(alphabet, strictMode)
}

class Caesar(val alphabet: Alphabet, val strictMode: Boolean = false) {

  @throws(classOf[DataCharNotInAlphabetException])
  def encrypt(key: Int, plaintext: CharSequence): String = 
    sumKeySeqWithText(_ => key)(plaintext, alphabet, strictMode, addByModulo)

  @throws(classOf[DataCharNotInAlphabetException])
  def decrypt(key: Int, ciphertext: CharSequence): String = 
    sumKeySeqWithText(_ => key)(ciphertext, alphabet, strictMode, subtractByModulo)

}
