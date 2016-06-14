package snakefish.crypto
package cipher.historical

import PolybiusSquare._
import Nihilist._

class NihilistTest extends BaseTest {
  
  private val plaintext  = "DYNAMITE WINTER PALACE"
  private val ciphertext = Array(37, 106, 62, 36, 67, 47, 86, 26, 104, 53, 62, 77, 27, 55, 57, 66, 55, 36, 54, 27)
  private val crKey = "RUSSIAN123"
  private val square = PolybiusSquare("ZEBRAS", Alphabet.ENGLISH, Map('J' -> 'I'))
  
  private val nonStrictCipher = Nihilist(square)
  private val strictCipher = Nihilist(square, true)
  
  ".encrypt" must "correctly encrypt plaintext using provided parameters" in {
    val _ciphertext = nonStrictCipher.encrypt(crKey, plaintext)
    _ciphertext must equal (ciphertext)
  }
  
  ".decrypt" must "correctly decrypt ciphertext using provided parameters" in {
    val _plaintext = nonStrictCipher.decrypt(crKey, ciphertext)
    _plaintext must be ("dynamitewinterpalace")
  }
  
  ".encrypt(strictMode)" must "throw an exception if plaintext contains char that is missing in Polybius square" in {
    val ex = the [DataCharNotInSquareException] thrownBy strictCipher.encrypt(crKey, plaintext)
    ex.position must be (plaintext.indexOf(' '))
  }
   
  ".decrypt" must "throw an exception if ciphertext contains wrong number" in {
    val wrongCiphertext = Array(37, -300, 106, 62, 36)
    val ex = the [CoordinatesOutOfBoundsException] thrownBy nonStrictCipher.decrypt(crKey, wrongCiphertext)
    ex.position must be (1)
  }
   
}
