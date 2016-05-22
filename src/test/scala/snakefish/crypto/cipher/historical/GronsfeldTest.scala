package snakefish.crypto.cipher.historical

import snakefish.crypto.BaseTest
import snakefish.crypto.data.Alphabet
import snakefish.crypto.data.DataCharNotInAlphabetException

class GronsfeldTest extends BaseTest {
  
  private val testPlainText = "GrOnS fElD"
  private val testEncodedText = "IrPsU fFqF"
  private val testKey = 2015
  
  ".encode" should "correctly encode data using provided key and alphabet" in {
    val encodedText = Gronsfeld.encode(testPlainText, testKey, Alphabet.ENGLISH)
    encodedText must be (testEncodedText.toCharArray)
  }
  
  ".decode" should "correctly decode data using provided key and alphabet" in {
    val plainText = Gronsfeld.decode(testEncodedText, testKey, Alphabet.ENGLISH)
    plainText must be (testPlainText.toCharArray)
  }
  
  ".encode & .decode" should "throw an exception in strict mode if income data contains symbols that are missing in alphabet" in {
    an [DataCharNotInAlphabetException] should be thrownBy Gronsfeld.encode(testPlainText, testKey, Alphabet.ENGLISH, true)
    an [DataCharNotInAlphabetException] should be thrownBy Gronsfeld.decode(testEncodedText, testKey, Alphabet.ENGLISH, true)
  }
  
}
