package snakefish.crypto.cipher.historical

import snakefish.crypto.BaseTest
import snakefish.crypto.data.Alphabet

class CaesarCipherTest extends BaseTest {
  
  private val testPlainText = "Съешь же ещё этих мягких французских булок, да выпей чаю."
  private val testEncodedText = "Фэзыя йз зьи ахлш пвёнлш чугрщцкфнлш дцосн, жг еютзм ъгб."
  private val testKey = 3
  
  ".encode" should "correctly encode data using provided key and alphabet" in {
    val encodedText = CaesarCipher.encode(testPlainText, testKey, Alphabet.RUSSIAN)
    encodedText must be (testEncodedText.toCharArray())
  }
  
  ".decode" should "correctly decode data using provided key and alphabet" in {
    val plainText = CaesarCipher.decode(testEncodedText, testKey, Alphabet.RUSSIAN)
    plainText must be (testPlainText.toCharArray())
  }
  
}
