package snakefish.crypto
package cipher.classical
package transposition

class DoubleSpec extends BaseSpec {
  
  private val plaintext  = "Hello World from Kiev"
  private val ciphertext = "leimrfloWo dvK leHr o"
  
  private val cipher = Double("TOMATO", "TOMATOS_123", Alphabet.ENGLISH)
  
  ".encrypt" must "correctly transpose plaintext" in {
    val _ciphertext = cipher.encrypt(plaintext)
    _ciphertext must be (ciphertext)
    
    cipher.encrypt("") must be ("")
  }
  
  ".decrypt" must "correctly transpose ciphertext" in {
    val _plaintext = cipher.decrypt(ciphertext)
    _plaintext must be (plaintext)
    
    cipher.decrypt("") must be ("")
  }
  
}
