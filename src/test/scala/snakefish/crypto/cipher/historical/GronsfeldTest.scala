package snakefish.crypto
package cipher.historical

class GronsfeldTest extends BaseTest {
  
  private val plaintext  = "GrOnS fElD"
  private val ciphertext = "IrPsU fFqF"
  private val crKey = 2015
  
  private val nonStrictCipher = Gronsfeld(Alphabet.ENGLISH)
  private val strictCipher = Gronsfeld(Alphabet.ENGLISH, true)
  
  ".encrypt" must "correctly encrypt plaintext using provided key and alphabet" in {
    val _ciphertext = nonStrictCipher.encrypt(crKey, plaintext)
    _ciphertext must be (ciphertext)
  }
  
  ".decrypt" must "correctly decrypt ciphertext using provided key and alphabet" in {
    val _plaintext = nonStrictCipher.decrypt(crKey, ciphertext)
    _plaintext must be (plaintext)
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
