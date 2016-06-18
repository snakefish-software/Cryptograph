package snakefish.crypto
package cipher.historical

class VigenereTest extends BaseTest {
  
  private val plaintext  = "Attack at dawn"
  private val ciphertext = "Lxfopv ef rnhr"
  private val crKey = "L e M o N"
  
  private val nonStrictCipher = Vigenere(crKey, Alphabet.ENGLISH)
  private val strictCipher = Vigenere(crKey, Alphabet.ENGLISH, true)
  
  ".encrypt" must "correctly encrypt plaintext" in {
    val _ciphertext = nonStrictCipher.encrypt(plaintext)
    _ciphertext must be (ciphertext)
  }
  
  ".decrypt" must "correctly decrypt ciphertext" in {
    val _plaintext = nonStrictCipher.decrypt(ciphertext)
    _plaintext must be (plaintext)
  }
  
  ".encrypt & .decrypt" must "left data as is if all key chars are missing in alphabet" in {
    val numKeyCipher = Vigenere("123456", Alphabet.ENGLISH)
    
    val _ciphertext = numKeyCipher.encrypt(plaintext)
    _ciphertext must be (plaintext)
    
    val _plaintext = numKeyCipher.decrypt(ciphertext)
    _plaintext must be (ciphertext)
  }
  
  ".encrypt(strictMode)" must "throw an exception if plaintext contains symbols that are missing in alphabet" in {
    val ex = the [DataCharNotInAlphabetException] thrownBy strictCipher.encrypt(plaintext)
    ex.position must be (plaintext.indexOf(' '))
  }
  
  ".decrypt(strictMode)" must "throw an exception if ciphertext contains symbols that are missing in alphabet" in {
    val ex = the [DataCharNotInAlphabetException] thrownBy strictCipher.decrypt(ciphertext)
    ex.position must be (ciphertext.indexOf(' '))
  }
  
}
