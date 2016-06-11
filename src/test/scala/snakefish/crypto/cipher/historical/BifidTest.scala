package snakefish.crypto
package cipher.historical

import PolybiusSquare._

class BifidTest extends BaseTest {
  
  private val plaintext = "Defend the east wall of the castle"
  private val cifertext = "Ffyhmk hyc plia shad tr lhc chlblr"
  private val period = 5
  private val square = PolybiusSquare(
      Array(Array('p', 'h', 'q', 'g', 'm'),
            Array('e', 'a', 'y', 'l', 'n'),
            Array('o', 'f', 'd', 'x', 'k'),
            Array('r', 'c', 'v', 's', 'z'),
            Array('w', 'b', 'u', 't', 'i')),
      Map('j' -> 'i'))
      
  private val nonStrictCifer = Bifid(square, period)
  private val strictCifer = Bifid(square, period, true)
  
  ".encode" must "correctly encode data using provided Polibius square and period" in {
    val _cifertext = nonStrictCifer.encode(plaintext)
    _cifertext must be (cifertext.toCharArray)
  }
  
  ".decode" must "correctly decode data using provided Polibius square and period" in {
    val _plaintext = nonStrictCifer.decode(cifertext)
    _plaintext must be (plaintext.toCharArray)
  }
  
  ".encode(strictMode)" must "throw an exception if income data contains symbols that are missing in square" in {
    val ex = the [DataCharNotInSquareException] thrownBy strictCifer.encode(plaintext)
    ex.position must be (plaintext.indexOf(' '))
  }
  
  ".decode(strictMode)" must "throw an exception if income data contains symbols that are missing in square" in {
    val ex = the [DataCharNotInSquareException] thrownBy strictCifer.decode(cifertext)
    ex.position must be (cifertext.indexOf(' '))
  }
  
}
