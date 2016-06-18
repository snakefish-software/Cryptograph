package snakefish.crypto
package cipher.historical

object Gronsfeld {
  
  def apply(key: Long, alphabet: Alphabet): Gronsfeld = 
    apply(key, alphabet, false)
  
  def apply(key: Long, alphabet: Alphabet, strictMode: Boolean): Gronsfeld = 
    new Gronsfeld(toDigits(key), alphabet, strictMode)
  
  def apply(key: Array[Int], alphabet: Alphabet): Gronsfeld = 
    apply(key, alphabet, false)
  
  def apply(key: Array[Int], alphabet: Alphabet, strictMode: Boolean): Gronsfeld = 
    new Gronsfeld(key, alphabet, strictMode)
}

class Gronsfeld(val key: Array[Int], val alphabet: Alphabet, val strictMode: Boolean = false) {
  
  @throws(classOf[DataCharNotInAlphabetException])
  def encrypt(plaintext: CharSequence): String = 
    sumKeySeqWithText(key, plaintext, alphabet, strictMode)(addByModulo)
  
  @throws(classOf[DataCharNotInAlphabetException])
  def decrypt(ciphertext: CharSequence): String = 
    sumKeySeqWithText(key, ciphertext, alphabet, strictMode)(subtractByModulo)
  
}
