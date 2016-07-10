package snakefish.crypto
package cipher.classical

import PolybiusSquare._

class BifidSpec extends BaseSpec {
  
  private val plaintext  = "Defend the east wall of the castle"
  private val ciphertext = "Ffyhmk hyc plia shad tr lhc chlblr"
  private val period = 5
  private val square = PolybiusSquare(
      Array(Array('p', 'h', 'q', 'g', 'm'),
            Array('e', 'a', 'y', 'l', 'n'),
            Array('o', 'f', 'd', 'x', 'k'),
            Array('r', 'c', 'v', 's', 'z'),
            Array('w', 'b', 'u', 't', 'i')),
      Map('j' -> 'i'))
      
  private val nonStrictCipher = Bifid(square, period)
  private val strictCipher = Bifid(square, period, true)
  
  ".encrypt" must "correctly encrypt plaintext" in {
    val _ciphertext = nonStrictCipher.encrypt(plaintext)
    _ciphertext must be (ciphertext)
    
    nonStrictCipher.encrypt("") must be ("")
  }
  
  it must "throw an exception in strict mode if plaintext contains symbols that are missing in square" in {
    val ex = the [DataCharNotInSquareException] thrownBy strictCipher.encrypt(plaintext)
    ex.char must be (' ')
    ex.position must be (plaintext.indexOf(' '))
  }
  
  ".decrypt" must "correctly decrypt ciphertext" in {
    val _plaintext = nonStrictCipher.decrypt(ciphertext)
    _plaintext must be (plaintext)
    
    nonStrictCipher.decrypt("") must be ("")
  }
  
  it must "throw an exception in strict mode if ciphertext contains symbols that are missing in square" in {
    val ex = the [DataCharNotInSquareException] thrownBy strictCipher.decrypt(ciphertext)
    ex.char must be (' ')
    ex.position must be (ciphertext.indexOf(' '))
  }
  
}
