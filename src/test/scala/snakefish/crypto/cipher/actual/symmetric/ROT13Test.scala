package snakefish.crypto.cipher.actual.symmetric

import snakefish.crypto.BaseTest
import snakefish.crypto.data.Alphabet
import snakefish.crypto.data.DataCharNotInAlphabetException

class ROT13Test extends BaseTest {
  
  private val testPlainText = "How can you tell an extrovert from an introvert at NSA? Va gur ryringbef, gur rkgebireg ybbxf ng gur BGURE thl'f fubrf."
  private val testEncodedText = "Ubj pna lbh gryy na rkgebireg sebz na vagebireg ng AFN? In the elevators, the extrovert looks at the OTHER guy's shoes."
  
  ".encode" should "correctly encode data using provided key and alphabet" in {
    val encodedText = ROT13.encode(testPlainText, Alphabet.ENGLISH)
    encodedText must be (testEncodedText.toCharArray())
  }
  
  ".decode" should "correctly decode data using provided key and alphabet" in {
    val plainText = ROT13.decode(testEncodedText, Alphabet.ENGLISH)
    plainText must be (testPlainText.toCharArray())
  }
  
  ".encode & .decode" should "throw an exception in strict mode if income data contains symbols that are missing in alphabet" in {
    an [DataCharNotInAlphabetException] should be thrownBy ROT13.encode(testPlainText, Alphabet.ENGLISH, true)
    an [DataCharNotInAlphabetException] should be thrownBy ROT13.decode(testEncodedText, Alphabet.ENGLISH, true)
  }
  
}
