package snakefish.crypto
package cipher.historical

class GronsfeldTest extends BaseTest {
  
  private val plaintext = "GrOnS fElD"
  private val cifertext = "IrPsU fFqF"
  private val crKey = 2015
  
  private val nonStrictCifer = Gronsfeld(Alphabet.ENGLISH)
  private val strictCifer = Gronsfeld(Alphabet.ENGLISH, true)
  
  ".encode" must "correctly encode data using provided key and alphabet" in {
    val _cifertext = nonStrictCifer.encode(plaintext, crKey)
    _cifertext must be (cifertext)
  }
  
  ".decode" must "correctly decode data using provided key and alphabet" in {
    val _plaintext = nonStrictCifer.decode(cifertext, crKey)
    _plaintext must be (plaintext)
  }
  
  ".encode(strictMode)" must "throw an exception if income data contains symbols that are missing in alphabet" in {
    val ex = the [DataCharNotInAlphabetException] thrownBy strictCifer.encode(plaintext, crKey)
    ex.position must be (plaintext.indexOf(' '))
  }
  
  ".decode(strictMode)" must "throw an exception if income data contains symbols that are missing in alphabet" in {
    val ex = the [DataCharNotInAlphabetException] thrownBy strictCifer.decode(cifertext, crKey)
    ex.position must be (cifertext.indexOf(' '))
  }
  
}
