package snakefish.crypto.cipher.historical

import snakefish.crypto.BaseTest
import snakefish.crypto.data.Alphabet
import snakefish.crypto.data.KeyCharNotInAlphabetException
import snakefish.crypto.data.DataCharNotInAlphabetException

class VigenereCipherTest extends BaseTest {
  
  private val testPlainText = "Attack at dawn"
  private val testEncodedText = "Lxfopv ef rnhr"
  private val testKey = "LeMoN"
  
  ".encode" should "correctly encode data using provided key and alphabet" in {
    val encodedText = Vigenere.encode(testPlainText, testKey, Alphabet.ENGLISH)
    encodedText must be (testEncodedText.toCharArray())
  }
  
  ".decode" should "correctly decode data using provided key and alphabet" in {
    val plainText = Vigenere.decode(testEncodedText, testKey, Alphabet.ENGLISH)
    plainText must be (testPlainText.toCharArray())
  }
  
  ".encode & .decode" should "thrown an exception if key char is missing in alphabet" in {
    an [KeyCharNotInAlphabetException] should be thrownBy Vigenere.encode(testPlainText, " ", Alphabet.ENGLISH)
    an [KeyCharNotInAlphabetException] should be thrownBy Vigenere.decode(testPlainText, " ", Alphabet.ENGLISH)
  }
  
  ".encode & .decode" should "throw an exception in strict mode if income data contains symbols that are missing in alphabet" in {
    an [DataCharNotInAlphabetException] should be thrownBy Vigenere.encode(testPlainText, testKey, Alphabet.ENGLISH, true)
    an [DataCharNotInAlphabetException] should be thrownBy Vigenere.decode(testEncodedText, testKey, Alphabet.ENGLISH, true)
  }
  
}
