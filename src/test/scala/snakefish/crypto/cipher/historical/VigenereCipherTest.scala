package snakefish.crypto.cipher.historical

import snakefish.crypto.BaseTest
import snakefish.crypto.data.Alphabet

class VigenereCipherTest extends BaseTest {
  
  private val testPlainText = "attackatdawn"
  private val testEncodedText = "lxfopvefrnhr"
  private val testKey = "lemon"
  
  ".encode" should "correctly encode data using provided key and alphabet" in {
    val encodedText = Vigenere.encode(testPlainText, testKey, Alphabet.ENGLISH)
    encodedText must be (testEncodedText.toCharArray())
  }
  
  ".decode" should "correctly decode data using provided key and alphabet" in {
    val plainText = Vigenere.decode(testEncodedText, testKey, Alphabet.ENGLISH)
    plainText must be (testPlainText.toCharArray())
  }
  
}
