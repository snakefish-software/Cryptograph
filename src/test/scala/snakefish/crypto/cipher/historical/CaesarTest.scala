package snakefish.crypto
package cipher.historical

class CaesarTest extends BaseTest {
  
  private val plaintext = "Съешь же ещё этих мягких французских булок, да выпей чаю."
  private val cifertext = "Фэзыя йз зьи ахлш пвёнлш чугрщцкфнлш дцосн, жг еютзм ъгб."
  private val crKey = 3
  
  ".encode" must "correctly encode data using provided key and alphabet" in {
    val _cifertext = Caesar.encode(plaintext, crKey, Alphabet.RUSSIAN)
    _cifertext must be (cifertext.toCharArray)
  }
  
  ".decode" must "correctly decode data using provided key and alphabet" in {
    val _plaintext = Caesar.decode(cifertext, crKey, Alphabet.RUSSIAN)
    _plaintext must be (plaintext.toCharArray)
  }
  
  ".encode(strictMode)" must "throw an exception if income data contains symbols that are missing in alphabet" in {
    val ex = the [DataCharNotInAlphabetException] thrownBy Caesar.encode(plaintext, crKey, Alphabet.RUSSIAN, true)
    ex.position must be (plaintext.indexOf(' '))
  }
  
  ".decode(strictMode)" must "throw an exception if income data contains symbols that are missing in alphabet" in {
    val ex = the [DataCharNotInAlphabetException] thrownBy Caesar.decode(cifertext, crKey, Alphabet.RUSSIAN, true)
    ex.position must be (cifertext.indexOf(' '))
  }
  
}
