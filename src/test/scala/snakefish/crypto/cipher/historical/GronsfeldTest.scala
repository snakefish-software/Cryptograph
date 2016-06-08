package snakefish.crypto
package cipher.historical

class GronsfeldTest extends BaseTest {
  
  private val plaintext = "GrOnS fElD"
  private val cifertext = "IrPsU fFqF"
  private val crKey = 2015
  
  ".encode" must "correctly encode data using provided key and alphabet" in {
    val _cifertext = Gronsfeld.encode(plaintext, crKey, Alphabet.ENGLISH)
    _cifertext must be (cifertext.toCharArray)
  }
  
  ".decode" must "correctly decode data using provided key and alphabet" in {
    val _plaintext = Gronsfeld.decode(cifertext, crKey, Alphabet.ENGLISH)
    _plaintext must be (plaintext.toCharArray)
  }
  
  ".encode(strictMode)" must "throw an exception if income data contains symbols that are missing in alphabet" in {
    val ex = the [DataCharNotInAlphabetException] thrownBy Gronsfeld.encode(plaintext, crKey, Alphabet.ENGLISH, true)
    ex.position must be (plaintext.indexOf(' '))
  }
  
  ".decode(strictMode)" must "throw an exception if income data contains symbols that are missing in alphabet" in {
    val ex = the [DataCharNotInAlphabetException] thrownBy Gronsfeld.decode(cifertext, crKey, Alphabet.ENGLISH, true)
    ex.position must be (cifertext.indexOf(' '))
  }
  
}
