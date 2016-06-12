package snakefish.crypto
package cipher.historical

class CaesarTest extends BaseTest {
  
  private val plaintext = "Съешь же ещё этих мягких французских булок, да выпей чаю."
  private val cifertext = "Фэзыя йз зьи ахлш пвёнлш чугрщцкфнлш дцосн, жг еютзм ъгб."
  private val crKey = 3
  
  private val nonStrictCifer = Caesar(Alphabet.RUSSIAN)
  private val strictCifer = Caesar(Alphabet.RUSSIAN, true)
  
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
