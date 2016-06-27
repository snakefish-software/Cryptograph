package snakefish.crypto
package cipher.symmetric

class ROT13Test extends BaseTest {
  
  private val plaintext  = "How can you tell an extrovert from an introvert at NSA? Va gur ryringbef, gur rkgebireg ybbxf ng gur BGURE thl'f fubrf."
  private val ciphertext = "Ubj pna lbh gryy na rkgebireg sebz na vagebireg ng AFN? In the elevators, the extrovert looks at the OTHER guy's shoes."
  
  private val nonStrictCipher = ROT13(Alphabet.ENGLISH)
  private val strictCipher = ROT13(Alphabet.ENGLISH, true)
  
  ".encrypt" must "correctly encrypt plaintext" in {
    val _ciphertext = nonStrictCipher.encrypt(plaintext)
    _ciphertext must be (ciphertext)
    
    nonStrictCipher.encrypt("") must be ("")
  }
  
  ".decrypt" must "correctly decrypt ciphertext" in {
    val _plaintext = nonStrictCipher.decrypt(ciphertext)
    _plaintext must be (plaintext)
    
    nonStrictCipher.decrypt("") must be ("")
  }
  
  ".encrypt(strictMode)" must "throw an exception if plaintext contains symbols that are missing in alphabet" in {
    val ex = the [DataCharNotInAlphabetException] thrownBy strictCipher.encrypt(plaintext)
    ex.position must be (plaintext.indexOf(' '))
  }
  
  ".decrypt(strictMode)" must "throw an exception if ciphertext contains symbols that are missing in alphabet" in {
    val ex = the [DataCharNotInAlphabetException] thrownBy strictCipher.decrypt(ciphertext)
    ex.position must be (ciphertext.indexOf(' '))
  }
  
}
