package snakefish.crypto
package cipher.historical

class VigenereTest extends BaseTest {
  
  private val plaintext = "Attack at dawn"
  private val cifertext = "Lxfopv ef rnhr"
  private val crKey = "L e M o N"
  
  ".encode" must "correctly encode data using provided key and alphabet" in {
    val _cifertext = Vigenere.encode(plaintext, crKey, Alphabet.ENGLISH)
    _cifertext must be (cifertext.toCharArray)
  }
  
  ".decode" must "correctly decode data using provided key and alphabet" in {
    val _plaintext = Vigenere.decode(cifertext, crKey, Alphabet.ENGLISH)
    _plaintext must be (plaintext.toCharArray)
  }
  
  ".encode & .decode" must "left data as is if all key chars are missing in alphabet" in {
    val _cifertext = Vigenere.encode(plaintext, "123456", Alphabet.ENGLISH)
    _cifertext must be (plaintext.toCharArray)
    
    val _plaintext = Vigenere.decode(cifertext, "123456", Alphabet.ENGLISH)
    _plaintext must be (cifertext.toCharArray)
  }
  
  ".encode(strictMode)" must "throw an exception if income data contains symbols that are missing in alphabet" in {
    val ex = the [DataCharNotInAlphabetException] thrownBy Vigenere.encode(plaintext, crKey, Alphabet.ENGLISH, true)
    ex.position must be (plaintext.indexOf(' '))
  }
  
  ".decode(strictMode)" must "throw an exception if income data contains symbols that are missing in alphabet" in {
    val ex = the [DataCharNotInAlphabetException] thrownBy Vigenere.decode(cifertext, crKey, Alphabet.ENGLISH, true)
    ex.position must be (cifertext.indexOf(' '))
  }
  
}
