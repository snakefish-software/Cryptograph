package snakefish.crypto
package cipher.classical

class VigenereTest extends BaseTest {
  
  private val plaintext  = "Attack at dawn"
  private val ciphertext = "Lxfopv ef rnhr"
  private val crKey = "L e M o N"
  
  private val nonStrictCipher = Vigenere(crKey, Alphabet.ENGLISH)
  private val strictCipher = Vigenere(crKey, Alphabet.ENGLISH, true)
  
  ".encrypt" must "correctly encrypt plaintext" in {
    val _ciphertext = nonStrictCipher.encrypt(plaintext)
    _ciphertext must be (ciphertext)
    
    nonStrictCipher.encrypt("") must be ("")
  }
  
  it must "throw an exception in strict mode if plaintext contains symbols that are missing in alphabet" in {
    val ex = the [DataCharNotInAlphabetException] thrownBy strictCipher.encrypt(plaintext)
    ex.position must be (plaintext.indexOf(' '))
  }
  
  ".decrypt" must "correctly decrypt ciphertext" in {
    val _plaintext = nonStrictCipher.decrypt(ciphertext)
    _plaintext must be (plaintext)
    
    nonStrictCipher.decrypt("") must be ("")
  }
  
  it must "throw an exception in strict mode if ciphertext contains symbols that are missing in alphabet" in {
    val ex = the [DataCharNotInAlphabetException] thrownBy strictCipher.decrypt(ciphertext)
    ex.position must be (ciphertext.indexOf(' '))
  }
  
  ".encrypt & .decrypt" must "left data as is if all key chars are missing in alphabet" in {
    val numKeyCipher = Vigenere("123456", Alphabet.ENGLISH)
    
    val _ciphertext = numKeyCipher.encrypt(plaintext)
    _ciphertext must be (plaintext)
    
    val _plaintext = numKeyCipher.decrypt(ciphertext)
    _plaintext must be (ciphertext)
  }
  
}
