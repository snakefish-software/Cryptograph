package snakefish.crypto
package cipher.actual.symmetric

class ROT13Test extends BaseTest {
  
  private val plaintext = "How can you tell an extrovert from an introvert at NSA? Va gur ryringbef, gur rkgebireg ybbxf ng gur BGURE thl'f fubrf."
  private val cifertext = "Ubj pna lbh gryy na rkgebireg sebz na vagebireg ng AFN? In the elevators, the extrovert looks at the OTHER guy's shoes."
  
  private val nonStrictCifer = ROT13(Alphabet.ENGLISH)
  private val strictCifer = ROT13(Alphabet.ENGLISH, true)
  
  ".encode" must "correctly encode data using provided key and alphabet" in {
    val _cifertext = nonStrictCifer.encode(plaintext)
    _cifertext must be (cifertext.toCharArray)
  }
  
  ".decode" must "correctly decode data using provided key and alphabet" in {
    val _plaintext = nonStrictCifer.decode(cifertext)
    _plaintext must be (plaintext.toCharArray)
  }
  
  ".encode(strictMode)" must "throw an exception if income data contains symbols that are missing in alphabet" in {
    val ex = the [DataCharNotInAlphabetException] thrownBy strictCifer.encode(plaintext)
    ex.position must be (plaintext.indexOf(' '))
  }
  
  ".decode(strictMode)" must "throw an exception if income data contains symbols that are missing in alphabet" in {
    val ex = the [DataCharNotInAlphabetException] thrownBy strictCifer.decode(cifertext)
    ex.position must be (cifertext.indexOf(' '))
  }
  
}
