package snakefish.crypto
package cipher.historical

import data.Alphabet
import data.KeyCharNotInAlphabetException
import data.DataCharNotInAlphabetException

class VigenereCipherTest extends BaseTest {
  
  private val plaintext = "Attack at dawn"
  private val cifertext = "Lxfopv ef rnhr"
  private val crKey = "LeMoN"
  
  ".encode" should "correctly encode data using provided key and alphabet" in {
    val _cifertext = Vigenere.encode(plaintext, crKey, Alphabet.ENGLISH)
    _cifertext must be (cifertext.toCharArray)
  }
  
  ".decode" should "correctly decode data using provided key and alphabet" in {
    val _plaintext = Vigenere.decode(cifertext, crKey, Alphabet.ENGLISH)
    _plaintext must be (plaintext.toCharArray)
  }
  
  ".encode & .decode" should "thrown an exception if key char is missing in alphabet" in {
    an [KeyCharNotInAlphabetException] should be thrownBy Vigenere.encode(plaintext, " ", Alphabet.ENGLISH)
    an [KeyCharNotInAlphabetException] should be thrownBy Vigenere.decode(cifertext, " ", Alphabet.ENGLISH)
  }
  
  ".encode & .decode" should "throw an exception in strict mode if income data contains symbols that are missing in alphabet" in {
    an [DataCharNotInAlphabetException] should be thrownBy Vigenere.encode(plaintext, crKey, Alphabet.ENGLISH, true)
    an [DataCharNotInAlphabetException] should be thrownBy Vigenere.decode(cifertext, crKey, Alphabet.ENGLISH, true)
  }
  
}
