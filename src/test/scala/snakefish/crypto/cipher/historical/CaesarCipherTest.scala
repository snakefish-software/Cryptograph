package snakefish.crypto.cipher.historical

import snakefish.crypto.BaseTest
import snakefish.crypto.data.Alphabet
import snakefish.crypto.data.DataCharNotInAlphabetException

class CaesarCipherTest extends BaseTest {
  
  private val testPlainText = "Съешь же ещё этих мягких французских булок, да выпей чаю."
  private val testEncodedText = "Фэзыя йз зьи ахлш пвёнлш чугрщцкфнлш дцосн, жг еютзм ъгб."
  private val testKey = 3
  
  ".encode" should "correctly encode data using provided key and alphabet" in {
    val encodedText = Caesar.encode(testPlainText, testKey, Alphabet.RUSSIAN)
    encodedText must be (testEncodedText.toCharArray)
  }
  
  ".decode" should "correctly decode data using provided key and alphabet" in {
    val plainText = Caesar.decode(testEncodedText, testKey, Alphabet.RUSSIAN)
    plainText must be (testPlainText.toCharArray)
  }
  
  ".encode & .decode" should "throw an exception in strict mode if income data contains symbols that are missing in alphabet" in {
    an [DataCharNotInAlphabetException] should be thrownBy Caesar.encode(testPlainText, testKey, Alphabet.RUSSIAN, true)
    an [DataCharNotInAlphabetException] should be thrownBy Caesar.decode(testEncodedText, testKey, Alphabet.RUSSIAN, true)
  }
  
}
