package snakefish.crypto.cipher.historical

import snakefish.crypto.BaseTest
import PolybiusSquare._

class BifidTest extends BaseTest {
  
  private val testPlainText = "Defend the east wall of the castle"
  private val testEncodedText = "Ffyhmk hyc plia shad tr lhc chlblr"
  private val period = 5
  private val square = PolybiusSquare(Array(Array('p', 'h', 'q', 'g', 'm'),
                                            Array('e', 'a', 'y', 'l', 'n'),
                                            Array('o', 'f', 'd', 'x', 'k'),
                                            Array('r', 'c', 'v', 's', 'z'),
                                            Array('w', 'b', 'u', 't', 'i')),
                                      Map('j' -> 'i'))
  
  ".encode" should "correctly encode data using provided Polibius square and period" in {
    val encodedText = Bifid.encode(testPlainText, square, period)
    encodedText must be (testEncodedText.toCharArray)
  }
  
  ".decode" should "correctly decode data using provided Polibius square and period" in {
    val plainText = Bifid.decode(testEncodedText, square, period)
    plainText must be (testPlainText.toCharArray)
  }
  
  ".encode & .decode" should "throw an exception in strict mode if income data contains symbols that are missing in provided square" in {
    an [DataCharNotInSquareException] should be thrownBy Bifid.encode(testPlainText, square, period, true)
    an [DataCharNotInSquareException] should be thrownBy Bifid.decode(testEncodedText, square, period, true)
  }
  
}
