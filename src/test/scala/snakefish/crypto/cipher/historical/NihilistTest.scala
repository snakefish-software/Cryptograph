package snakefish.crypto.cipher.historical

import snakefish.crypto.BaseTest
import PolybiusSquare._
import snakefish.crypto.data.Alphabet

class NihilistTest extends BaseTest {
  
  private val square = PolybiusSquare("ZEBRAS", Alphabet.ENGLISH, Map('J' -> 'I'))
  private val ciferText = Array(37, 106, 62, 36, 67, 47, 86, 26, 104, 53, 62, 77, 27, 55, 57, 66, 55, 36, 54, 27)
  
  ".encode" should "correctly encode data using provided parameters" in {
    val encoded = Nihilist.encode("DYNAMITE WINTER PALACE", "RUSSIAN", square)
    encoded must equal (ciferText)
  }
  
  ".decode" should "correctly decode data using provided parameters" in {
    val plainText = Nihilist.decode(ciferText, "RUSSIAN", square)
    plainText must be ("dynamitewinterpalace".toCharArray)
  }
  
}
