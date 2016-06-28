package snakefish.crypto
package cipher.classical
package transposition

object Double {
  
  def apply(key1: CharSequence, key2: CharSequence, alphabet: Alphabet) = 
    new Double(key1, key2, alphabet)
}

class Double(key1: CharSequence, key2: CharSequence, alphabet: Alphabet) {
  
  private val columnar1 = Columnar(key1, alphabet)
  private val columnar2 = Columnar(key2, alphabet)
  
  def encrypt(plaintext: CharSequence): String = {
    columnar2.encrypt(columnar1.encrypt(plaintext))
  }
  
  def decrypt(ciphertext: CharSequence): String = {
    columnar1.decrypt(columnar2.decrypt(ciphertext))
  }
  
}
