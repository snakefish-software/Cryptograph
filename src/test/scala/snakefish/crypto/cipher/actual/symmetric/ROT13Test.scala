package snakefish.crypto
package cipher.actual.symmetric

import data.Alphabet
import data.DataCharNotInAlphabetException

class ROT13Test extends BaseTest {
  
  private val plaintext = "How can you tell an extrovert from an introvert at NSA? Va gur ryringbef, gur rkgebireg ybbxf ng gur BGURE thl'f fubrf."
  private val cifertext = "Ubj pna lbh gryy na rkgebireg sebz na vagebireg ng AFN? In the elevators, the extrovert looks at the OTHER guy's shoes."
  
  ".encode" should "correctly encode data using provided key and alphabet" in {
    val _cifertext = ROT13.encode(plaintext, Alphabet.ENGLISH)
    _cifertext must be (cifertext.toCharArray)
  }
  
  ".decode" should "correctly decode data using provided key and alphabet" in {
    val _plaintext = ROT13.decode(cifertext, Alphabet.ENGLISH)
    _plaintext must be (plaintext.toCharArray)
  }
  
  ".encode & .decode" should "throw an exception in strict mode if income data contains symbols that are missing in alphabet" in {
    an [DataCharNotInAlphabetException] should be thrownBy ROT13.encode(plaintext, Alphabet.ENGLISH, true)
    an [DataCharNotInAlphabetException] should be thrownBy ROT13.decode(cifertext, Alphabet.ENGLISH, true)
  }
  
}
