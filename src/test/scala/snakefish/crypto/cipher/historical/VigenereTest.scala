package snakefish.crypto
package cipher.historical

class VigenereTest extends BaseTest {
  
  private val plaintext = "Attack at dawn"
  private val cifertext = "Lxfopv ef rnhr"
  private val crKey = "L e M o N"
  
  private val nonStrictCifer = Vigenere(Alphabet.ENGLISH)
  private val strictCifer = Vigenere(Alphabet.ENGLISH, true)
  
  ".encode" must "correctly encode data using provided key and alphabet" in {
    val _cifertext = nonStrictCifer.encode(plaintext, crKey)
    _cifertext must be (cifertext)
  }
  
  ".decode" must "correctly decode data using provided key and alphabet" in {
    val _plaintext = nonStrictCifer.decode(cifertext, crKey)
    _plaintext must be (plaintext)
  }
  
  ".encode & .decode" must "left data as is if all key chars are missing in alphabet" in {
    val _cifertext = nonStrictCifer.encode(plaintext, "123456")
    _cifertext must be (plaintext)
    
    val _plaintext = nonStrictCifer.decode(cifertext, "123456")
    _plaintext must be (cifertext)
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
