package snakefish.crypto
package cipher.historical

import data.Alphabet
import data.DataCharNotInAlphabetException

class GronsfeldTest extends BaseTest {
  
  private val plaintext = "GrOnS fElD"
  private val cifertext = "IrPsU fFqF"
  private val crKey = 2015
  
  ".encode" should "correctly encode data using provided key and alphabet" in {
    val _cifertext = Gronsfeld.encode(plaintext, crKey, Alphabet.ENGLISH)
    _cifertext must be (cifertext.toCharArray)
  }
  
  ".decode" should "correctly decode data using provided key and alphabet" in {
    val _plaintext = Gronsfeld.decode(cifertext, crKey, Alphabet.ENGLISH)
    _plaintext must be (plaintext.toCharArray)
  }
  
  ".encode & .decode" should "throw an exception in strict mode if income data contains symbols that are missing in alphabet" in {
    an [DataCharNotInAlphabetException] should be thrownBy Gronsfeld.encode(plaintext, crKey, Alphabet.ENGLISH, true)
    an [DataCharNotInAlphabetException] should be thrownBy Gronsfeld.decode(cifertext, crKey, Alphabet.ENGLISH, true)
  }
  
}
