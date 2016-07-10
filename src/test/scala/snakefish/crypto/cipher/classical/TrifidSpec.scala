package snakefish.crypto
package cipher.classical

import Trifid._

class TrifidSpec extends BaseSpec {
  
  private val plaintext  = "Defend the east wall of the castle."
  private val ciphertext = "Suefec phs egyy jixi mf ofo cejlbsp"
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
                                 
  private val nonStrictCipher = Trifid(cube, period)
  private val strictCipher = Trifid(cube, period, true)
                                 
  "DataCharNotInCubeException" must "have correct exception message" in {
    val ex = new DataCharNotInCubeException('a', 5)
    ex.getMessage must be ("Char 'a' at position 5 is missing in cube")
  }
  
  "CoordinatesOutOfBoundsException" must "have correct exception message" in {
    val ex = new CoordinatesOutOfBoundsException(5, -1, -2 ,-3)
    ex.getMessage must be ("Coordinates (table = -1; row = -2; column = -3) of char at position 5 are out of cube bounds")
  }
  
  ".encrypt" must "correctly encrypt plaintext" in {
    val _ciphertext = nonStrictCipher.encrypt(plaintext)
    _ciphertext must be (ciphertext)
    
    nonStrictCipher.encrypt("") must be ("")
  }
  
  it must "throw an exception in strict mode if plaintext contains symbols that are missing in cube" in {
    val ex = the [DataCharNotInCubeException] thrownBy strictCipher.encrypt(plaintext)
    ex.position must be (plaintext.indexOf(' '))
  }
  
  ".decrypt" must "correctly decrypt ciphertext" in {
    val _plaintext = nonStrictCipher.decrypt(ciphertext)
    _plaintext must be (plaintext)
    
    nonStrictCipher.decrypt("") must be ("")
  }
  
  it must "throw an exception in strict mode if ciphertext contains symbols that are missing in cube" in {
    val ex = the [DataCharNotInCubeException] thrownBy strictCipher.decrypt(ciphertext)
    ex.char must be (' ')
    ex.position must be (ciphertext.indexOf(' '))
  }
  
}
