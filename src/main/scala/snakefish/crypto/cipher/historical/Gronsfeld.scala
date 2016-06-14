package snakefish.crypto
package cipher.historical

object Gronsfeld {
  
  def apply(alphabet: Alphabet, strictMode: Boolean = false) = 
    new Gronsfeld(alphabet, strictMode)
}

class Gronsfeld(val alphabet: Alphabet, val strictMode: Boolean = false) {
  
  @throws(classOf[DataCharNotInAlphabetException])
  def encrypt(key: Long, plaintext: CharSequence): String = 
    encrypt(toDigits(key), plaintext)
  
  @throws(classOf[DataCharNotInAlphabetException])
  def encrypt(key: Array[Int], plaintext: CharSequence): String = 
    sumKeySeqWithText(key, plaintext, alphabet, strictMode)(addByModulo)
  
  @throws(classOf[DataCharNotInAlphabetException])
  def decrypt(key: Long, ciphertext: CharSequence): String = 
    decrypt(toDigits(key), ciphertext)
  
  @throws(classOf[DataCharNotInAlphabetException])
  def decrypt(key: Array[Int], ciphertext: CharSequence): String = 
    sumKeySeqWithText(key, ciphertext, alphabet, strictMode)(subtractByModulo)
  
}
