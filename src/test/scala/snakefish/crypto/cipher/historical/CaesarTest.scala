package snakefish.crypto
package cipher.historical

class CaesarTest extends BaseTest {
  
  private val plaintext  = "Съешь же ещё этих мягких французских булок, да выпей чаю."
  private val ciphertext = "Фэзыя йз зьи ахлш пвёнлш чугрщцкфнлш дцосн, жг еютзм ъгб."
  private val crKey = 3
  
  private val nonStrictCipher = Caesar(Alphabet.RUSSIAN)
  private val strictCipher = Caesar(Alphabet.RUSSIAN, true)
  
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
