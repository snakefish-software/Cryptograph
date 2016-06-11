package snakefish.crypto
package cipher.historical

import Trifid._

class TrifidTest extends BaseTest {
  
  private val plaintext = "Defend the east wall of the castle."
  private val cifertext = "Suefec phs egyy jixi mf ofo cejlbsp"
  private val period = 5
  private val cube = Array(Array(Array('e', 'p', 's'),
                                 Array('d', 'u', 'c'),
                                 Array('v', 'w', 'y')),
                                 
                           Array(Array('m', '.', 'z'),
                                 Array('l', 'k', 'x'),
                                 Array('n', 'b', 't')),
                           
                           Array(Array('f', 'g', 'o'),
                                 Array('r', 'i', 'j'),
                                 Array('h', 'a', 'q')))
                                 
  private val nonStrictCifer = Trifid(cube, period)
  private val strictCifer = Trifid(cube, period, true)
                                 
  "DataCharNotInCubeException" must "have correct exception message" in {
    val ex = new DataCharNotInCubeException(5)
    ex.getMessage must be ("Data char at position 5 is missing in cube")
  }
  
  "CoordinatesOutOfBoundsException" must "have correct exception message" in {
    val ex = new CoordinatesOutOfBoundsException(5, -1, -2 ,-3)
    ex.getMessage must be ("Coordinates (table = -1; row = -2; column = -3) of char at position 5 are out of cube bounds")
  }
  
  ".encode" must "correctly encode data using provided parameters" in {
    val _cifertext = nonStrictCifer.encode(plaintext)
    _cifertext must be (cifertext.toCharArray)
  }
  
  ".decode" must "correctly decode cifertext using provided parameters" in {
    val _plaintext = nonStrictCifer.decode(cifertext)
    _plaintext must be (plaintext.toCharArray)
  }
  
  ".encode(strictMode)" must "throw an exception if plaintext contains symbols that are missing in cube" in {
    val ex = the [DataCharNotInCubeException] thrownBy strictCifer.encode(plaintext)
    ex.position must be (plaintext.indexOf(' '))
  }
  
  ".decode(strictMode)" must "throw an exception if cifertext contains symbols that are missing in cube" in {
    val ex = the [DataCharNotInCubeException] thrownBy strictCifer.decode(cifertext)
    ex.position must be (cifertext.indexOf(' '))
  }
  
}
