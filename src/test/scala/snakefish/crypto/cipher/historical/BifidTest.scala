package snakefish.crypto
package cipher.historical

import PolybiusSquare._

class BifidTest extends BaseTest {
  
  private val plaintext = "Defend the east wall of the castle"
  private val cifertext = "Ffyhmk hyc plia shad tr lhc chlblr"
  private val period = 5
  private val square = PolybiusSquare(Array(Array('p', 'h', 'q', 'g', 'm'),
                                            Array('e', 'a', 'y', 'l', 'n'),
                                            Array('o', 'f', 'd', 'x', 'k'),
                                            Array('r', 'c', 'v', 's', 'z'),
                                            Array('w', 'b', 'u', 't', 'i')),
                                      Map('j' -> 'i'))
  
  ".encode" must "correctly encode data using provided Polibius square and period" in {
    val _cifertext = Bifid.encode(plaintext, square, period)
    _cifertext must be (cifertext.toCharArray)
  }
  
  ".decode" must "correctly decode data using provided Polibius square and period" in {
    val _plaintext = Bifid.decode(cifertext, square, period)
    _plaintext must be (plaintext.toCharArray)
  }
  
  ".encode & .decode" must "throw an exception in strict mode if income data contains symbols that are missing in provided square" in {
    an [DataCharNotInSquareException] must be thrownBy Bifid.encode(plaintext, square, period, true)
    an [DataCharNotInSquareException] must be thrownBy Bifid.decode(cifertext, square, period, true)
  }
  
}
