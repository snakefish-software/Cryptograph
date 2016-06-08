package snakefish.crypto
package cipher.historical

import PolybiusSquare._
import Nihilist._

class NihilistTest extends BaseTest {
  
  private val plaintext = "DYNAMITE WINTER PALACE"
  private val cifertext = Array(37, 106, 62, 36, 67, 47, 86, 26, 104, 53, 62, 77, 27, 55, 57, 66, 55, 36, 54, 27)
  private val crKey = "RUSSIAN123"
  private val square = PolybiusSquare("ZEBRAS", Alphabet.ENGLISH, Map('J' -> 'I'))
  
  ".encode" must "correctly encode data using provided parameters" in {
    val _cifertext = Nihilist.encode(plaintext, crKey, square)
    _cifertext must equal (cifertext)
  }
  
  ".decode" must "correctly decode data using provided parameters" in {
    val _plaintext = Nihilist.decode(cifertext, crKey, square)
    _plaintext must be ("dynamitewinterpalace".toCharArray)
  }
  
  ".encode(strictMode)" must "throw an exception if data contains char that is missing in Polybius square" in {
    val ex = the [DataCharNotInSquareException] thrownBy Nihilist.encode(plaintext, crKey, square, true)
    ex.position must be (plaintext.indexOf(' '))
  }
   
  ".decode" must "throw an exception if cifertext contains wrong number" in {
    val wrongCifertext = Array(37, -300, 106, 62, 36)
    val ex = the [CoordinatesOutOfBoundsException] thrownBy Nihilist.decode(wrongCifertext, crKey, square)
    ex.position must be (1)
  }
   
}
