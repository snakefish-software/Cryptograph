package snakefish.crypto
package cipher.classical
package transposition

class ColumnarTest extends BaseTest {
  
  private val plaintext1  = "Hello World from Kiev"
  private val ciphertext1 = "llmlroveore  KHWfiod "
  
  private val plaintext2  = "Hello "
  private val ciphertext2 = "lle Ho"
  
  private val cipher = Columnar("TOMATO", Alphabet.ENGLISH)
  
  ".encrypt" must "correctly transpose plaintext" in {
    val _ciphertext1 = cipher.encrypt(plaintext1)
    _ciphertext1 must be (ciphertext1)
    
    val _ciphertext2 = cipher.encrypt(plaintext2)
    _ciphertext2 must be (ciphertext2)
    
    cipher.encrypt("") must be ("")
  }
  
  ".decrypt" must "correctly transpose ciphertext" in {
    val _plaintext1 = cipher.decrypt(ciphertext1)
    _plaintext1 must be (plaintext1)
    
    val _plaintext2 = cipher.decrypt(ciphertext2)
    _plaintext2 must be (plaintext2)
    
    cipher.decrypt("") must be ("")
  }
  
}
