package snakefish.crypto
package cipher.symmetric

object ROT13 {
  
  def apply(alphabet: Alphabet, strictMode: Boolean = false) = 
    new ROT13(alphabet, strictMode)
}

class ROT13(val alphabet: Alphabet, val strictMode: Boolean = false) {

  @throws(classOf[DataCharNotInAlphabetException])
  def encrypt(plaintext: CharSequence): String = 
    sumKeySeqWithText(_ => 13)(plaintext, alphabet, strictMode, addByModulo)

  @throws(classOf[DataCharNotInAlphabetException])
  def decrypt(ciphertext: CharSequence): String = 
    sumKeySeqWithText(_ => 13)(ciphertext, alphabet, strictMode, subtractByModulo)
  
}
