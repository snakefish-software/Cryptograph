package snakefish.crypto
package cipher.actual.symmetric

class ROT13Test extends BaseTest {
  
  private val plaintext = "How can you tell an extrovert from an introvert at NSA? Va gur ryringbef, gur rkgebireg ybbxf ng gur BGURE thl'f fubrf."
  private val cifertext = "Ubj pna lbh gryy na rkgebireg sebz na vagebireg ng AFN? In the elevators, the extrovert looks at the OTHER guy's shoes."
  
  ".encode" must "correctly encode data using provided key and alphabet" in {
    val _cifertext = ROT13.encode(plaintext, Alphabet.ENGLISH)
    _cifertext must be (cifertext.toCharArray)
  }
  
  ".decode" must "correctly decode data using provided key and alphabet" in {
    val _plaintext = ROT13.decode(cifertext, Alphabet.ENGLISH)
    _plaintext must be (plaintext.toCharArray)
  }
  
  ".encode(strictMode)" must "throw an exception if income data contains symbols that are missing in alphabet" in {
    val ex = the [DataCharNotInAlphabetException] thrownBy ROT13.encode(plaintext, Alphabet.ENGLISH, true)
    ex.position must be (plaintext.indexOf(' '))
  }
  
  ".decode(strictMode)" must "throw an exception if income data contains symbols that are missing in alphabet" in {
    val ex = the [DataCharNotInAlphabetException] thrownBy ROT13.decode(cifertext, Alphabet.ENGLISH, true)
    ex.position must be (cifertext.indexOf(' '))
  }
  
}
