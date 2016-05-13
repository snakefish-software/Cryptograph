package snakefish.crypto.cipher.actual.symmetric

import snakefish.crypto.BaseTest
import snakefish.crypto.data.Alphabet

class ROT13CipherTest extends BaseTest {
  
  private val testPlainText = "How can you tell an extrovert from an introvert at NSA? Va gur ryringbef, gur rkgebireg ybbxf ng gur BGURE thl'f fubrf."
  private val testEncodedText = "Ubj pna lbh gryy na rkgebireg sebz na vagebireg ng AFN? In the elevators, the extrovert looks at the OTHER guy's shoes."
  
  ".encode" should "correctly encode data using provided key and alphabet" in {
    val encodedText = ROT13Cipher.encode(testPlainText, Alphabet.ENGLISH)
    encodedText must be (testEncodedText)
  }
  
  ".decode" should "correctly decode data using provided key and alphabet" in {
    val plainText = ROT13Cipher.decode(testEncodedText, Alphabet.ENGLISH)
    plainText must be (testPlainText)
  }
  
}
