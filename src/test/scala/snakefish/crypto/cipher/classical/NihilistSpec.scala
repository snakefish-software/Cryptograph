package snakefish.crypto
package cipher.classical

import PolybiusSquare._
import Nihilist._

class NihilistSpec extends BaseSpec {
  
  private val plaintext  = "DYNAMITE WINTER PALACE"
  private val ciphertext = Array(37, 106, 62, 36, 67, 47, 86, 26, 104, 53, 62, 77, 27, 55, 57, 66, 55, 36, 54, 27)
  private val crKey = "RUSSIAN123"
  private val square = PolybiusSquare("ZEBRAS", Alphabet.ENGLISH, Map('J' -> 'I'))
  
  private val nonStrictCipher = Nihilist(crKey, square)
  private val strictCipher = Nihilist(crKey, square, true)
  
  ".encrypt" must "correctly encrypt plaintext" in {
    val _ciphertext = nonStrictCipher.encrypt(plaintext)
    _ciphertext must equal (ciphertext)
    
    nonStrictCipher.encrypt("") must equal (Array[Int]())
  }
  
  it must "throw an exception in strict mode if plaintext contains char that is missing in Polybius square" in {
    val ex = the [DataCharNotInSquareException] thrownBy strictCipher.encrypt(plaintext)
    ex.char must be (' ')
    ex.position must be (plaintext.indexOf(' '))
  }
  
  ".decrypt" must "correctly decrypt ciphertext" in {
    val _plaintext = nonStrictCipher.decrypt(ciphertext)
    _plaintext must be ("dynamitewinterpalace")
    
    nonStrictCipher.decrypt(Array[Int]()) must be ("")
  }
   
  it must "throw an exception in strict mode if ciphertext contains wrong number" in {
    val wrongCiphertext = Array(37, -300, 106, 62, 36)
    val ex = the [CoordinatesOutOfBoundsException] thrownBy strictCipher.decrypt(wrongCiphertext)
    ex.position must be (1)
  }
   
}
