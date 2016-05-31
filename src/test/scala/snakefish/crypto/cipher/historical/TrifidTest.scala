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
  
  ".encode" should "correctly encode data using provided parameters" in {
    val _cifertext = Trifid.encode(plaintext, cube, period)
    _cifertext must be (cifertext.toCharArray)
  }
  
  ".decode" should "correctly decode cifertext using provided parameters" in {
    val _plaintext = Trifid.decode(cifertext, cube, period)
    _plaintext must be (plaintext.toCharArray)
  }
  
  ".encode & .decode" should "throw an exception in strict mode if income data contains symbols that are missing in provided square" in {
    an [DataCharNotInCubeException] should be thrownBy Trifid.encode(plaintext, cube, period, true)
    an [DataCharNotInCubeException] should be thrownBy Trifid.decode(cifertext, cube, period, true)
  }
  
}
