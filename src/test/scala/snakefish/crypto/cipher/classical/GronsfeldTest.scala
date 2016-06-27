package snakefish.crypto
package cipher.classical

class GronsfeldTest extends BaseTest {
  
  private val plaintext  = "GrOnS fElD"
  private val ciphertext = "IrPsU fFqF"
  
  private val nonStrictCipher = Gronsfeld(2015, Alphabet.ENGLISH)
  private val strictCipher = Gronsfeld(2015, Alphabet.ENGLISH, true)
  
  ".encrypt" must "correctly encrypt plaintext" in {
    val _ciphertext = nonStrictCipher.encrypt(plaintext)
    _ciphertext must be (ciphertext)
  }
  
  ".decrypt" must "correctly decrypt ciphertext" in {
    val _plaintext = nonStrictCipher.decrypt(ciphertext)
    _plaintext must be (plaintext)
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
