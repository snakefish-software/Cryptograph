package snakefish.crypto
package cipher.historical

import data.Alphabet
import data.DataCharNotInAlphabetException

class CaesarCipherTest extends BaseTest {
  
  private val plaintext = "Съешь же ещё этих мягких французских булок, да выпей чаю."
  private val cifertext = "Фэзыя йз зьи ахлш пвёнлш чугрщцкфнлш дцосн, жг еютзм ъгб."
  private val crKey = 3
  
  ".encode" should "correctly encode data using provided key and alphabet" in {
    val _cifertext = Caesar.encode(plaintext, crKey, Alphabet.RUSSIAN)
    _cifertext must be (cifertext.toCharArray)
  }
  
  ".decode" should "correctly decode data using provided key and alphabet" in {
    val _plaintext = Caesar.decode(cifertext, crKey, Alphabet.RUSSIAN)
    _plaintext must be (plaintext.toCharArray)
  }
  
  ".encode & .decode" should "throw an exception in strict mode if income data contains symbols that are missing in alphabet" in {
    an [DataCharNotInAlphabetException] should be thrownBy Caesar.encode(plaintext, crKey, Alphabet.RUSSIAN, true)
    an [DataCharNotInAlphabetException] should be thrownBy Caesar.decode(cifertext, crKey, Alphabet.RUSSIAN, true)
  }
  
}
