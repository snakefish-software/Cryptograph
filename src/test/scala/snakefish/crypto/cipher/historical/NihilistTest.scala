package snakefish.crypto.cipher.historical

import snakefish.crypto.BaseTest
import PolybiusSquare._
import Nihilist._
import snakefish.crypto.data.Alphabet

class NihilistTest extends BaseTest {
  
  private val square = PolybiusSquare("ZEBRAS", Alphabet.ENGLISH, Map('J' -> 'I'))
  private val ciferKey = "RUSSIAN"
  private val cifertext = Array(37, 106, 62, 36, 67, 47, 86, 26, 104, 53, 62, 77, 27, 55, 57, 66, 55, 36, 54, 27)
  
  ".encode" should "correctly encode data using provided parameters" in {
    val encoded = Nihilist.encode("DYNAMITE WINTER PALACE", ciferKey, square)
    encoded must equal (cifertext)
  }
  
  ".decode" should "correctly decode data using provided parameters" in {
    val plainText = Nihilist.decode(cifertext, ciferKey, square)
    plainText must be ("dynamitewinterpalace".toCharArray)
  }
  
   ".encode and .decode" should "throw an exception if key contains chars that are missing in square" in {
     an [KeyCharNotInSquareException] should be thrownBy Nihilist.encode("DYNAMITE WINTER PALACE", "1234", square)
     an [KeyCharNotInSquareException] should be thrownBy Nihilist.decode(cifertext, "1234", square)
   }
   
   ".decode" should "throw an exception if cifertext contains wrong number" in {
     val wrongCifertext = Array(100500, -1000)
     an [CifertextNumberException] should be thrownBy Nihilist.decode(wrongCifertext, ciferKey, square)
   }
   
}
