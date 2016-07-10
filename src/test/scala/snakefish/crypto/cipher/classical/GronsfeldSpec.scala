package snakefish.crypto
package cipher.classical

class GronsfeldSpec extends BaseSpec {
  
  private val plaintext  = "GrOnS fElD"
  private val ciphertext = "IrPsU fFqF"
  
  private val nonStrictCipher = Gronsfeld(2015, Alphabet.ENGLISH)
  private val strictCipher = Gronsfeld(2015, Alphabet.ENGLISH, true)
  
  ".encrypt" must "correctly encrypt plaintext" in {
    val _ciphertext = nonStrictCipher.encrypt(plaintext)
    _ciphertext must be (ciphertext)
    
    nonStrictCipher.encrypt("") must be ("")
  }
  
  it must "throw an exception in strict mode if plaintext contains symbols that are missing in alphabet" in {
    val ex = the [DataCharNotInAlphabetException] thrownBy strictCipher.encrypt(plaintext)
    ex.char must be (' ')
    ex.position must be (plaintext.indexOf(' '))
  }
  
  ".decrypt" must "correctly decrypt ciphertext" in {
    val _plaintext = nonStrictCipher.decrypt(ciphertext)
    _plaintext must be (plaintext)
    
    nonStrictCipher.decrypt("") must be ("")
  }
  
  it must "throw an exception in strict mode if ciphertext contains symbols that are missing in alphabet" in {
    val ex = the [DataCharNotInAlphabetException] thrownBy strictCipher.decrypt(ciphertext)
    ex.char must be (' ')
    ex.position must be (ciphertext.indexOf(' '))
  }
  
}
