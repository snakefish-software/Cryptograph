package snakefish.crypto
package cipher.historical

class VigenereTest extends BaseTest {
  
  private val plaintext  = "Attack at dawn"
  private val ciphertext = "Lxfopv ef rnhr"
  private val crKey = "L e M o N"
  
  private val nonStrictCipher = Vigenere(Alphabet.ENGLISH)
  private val strictCipher = Vigenere(Alphabet.ENGLISH, true)
  
  ".encrypt" must "correctly encrypt plaintext using provided key and alphabet" in {
    val _ciphertext = nonStrictCipher.encrypt(crKey, plaintext)
    _ciphertext must be (ciphertext)
  }
  
  ".decrypt" must "correctly decrypt ciphertext using provided key and alphabet" in {
    val _plaintext = nonStrictCipher.decrypt(crKey, ciphertext)
    _plaintext must be (plaintext)
  }
  
  ".encrypt & .decrypt" must "left data as is if all key chars are missing in alphabet" in {
    val _ciphertext = nonStrictCipher.encrypt("123456", plaintext)
    _ciphertext must be (plaintext)
    
    val _plaintext = nonStrictCipher.decrypt("123456", ciphertext)
    _plaintext must be (ciphertext)
  }
  
  ".encrypt(strictMode)" must "throw an exception if income plaintext contains symbols that are missing in alphabet" in {
    val ex = the [DataCharNotInAlphabetException] thrownBy strictCipher.encrypt(crKey, plaintext)
    ex.position must be (plaintext.indexOf(' '))
  }
  
  ".decrypt(strictMode)" must "throw an exception if income ciphertext contains symbols that are missing in alphabet" in {
    val ex = the [DataCharNotInAlphabetException] thrownBy strictCipher.decrypt(crKey, ciphertext)
    ex.position must be (ciphertext.indexOf(' '))
  }
  
}
