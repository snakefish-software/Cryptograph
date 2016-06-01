package snakefish.crypto
package cipher.historical

import PolybiusSquare._
import Nihilist._

class NihilistTest extends BaseTest {
  
  private val cifertext = Array(37, 106, 62, 36, 67, 47, 86, 26, 104, 53, 62, 77, 27, 55, 57, 66, 55, 36, 54, 27)
  private val crKey = "RUSSIAN"
  private val square = PolybiusSquare("ZEBRAS", Alphabet.ENGLISH, Map('J' -> 'I'))
  
  ".encode" should "correctly encode data using provided parameters" in {
    val _cifertext = Nihilist.encode("DYNAMITE WINTER PALACE", crKey, square)
    _cifertext must equal (cifertext)
  }
  
  ".decode" should "correctly decode data using provided parameters" in {
    val _plaintext = Nihilist.decode(cifertext, crKey, square)
    _plaintext must be ("dynamitewinterpalace".toCharArray)
  }
  
   ".encode and .decode" should "throw an exception if key contains chars that are missing in square" in {
     an [KeyCharNotInSquareException] should be thrownBy Nihilist.encode("DYNAMITE WINTER PALACE", "1234", square)
     an [KeyCharNotInSquareException] should be thrownBy Nihilist.decode(cifertext, "1234", square)
   }
   
   ".decode" should "throw an exception if cifertext contains wrong number" in {
     val wrongCifertext = Array(100500, -1000)
     an [CifertextNumberException] should be thrownBy Nihilist.decode(wrongCifertext, crKey, square)
   }
   
}
