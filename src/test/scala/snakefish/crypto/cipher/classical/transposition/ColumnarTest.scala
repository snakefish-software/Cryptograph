package snakefish.crypto
package cipher.classical
package transposition

class ColumnarTest extends BaseTest {
  
  private val plaintext  = "Hello World from Kiev"
  private val ciphertext = "llmlroveore  KHWfiod "
  
  private val cipher = Columnar("TOMATO", Alphabet.ENGLISH)
  
  ".encrypt" must "correctly transpose plaintext" in {
    val _ciphertext = cipher.encrypt(plaintext)
    _ciphertext must be (ciphertext)
    
    cipher.encrypt("") must be ("")
  }
  
}
