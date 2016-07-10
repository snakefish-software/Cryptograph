package snakefish.crypto
package cipher.classical

import ADFGX._
import PolybiusSquare._

class ADFGXSpec extends BaseSpec {
      
  private val square = PolybiusSquare(
      Array(Array('F', 'N', 'H', 'E', 'Q'),
            Array('R', 'D', 'Z', 'O', 'C'),
            Array('I', 'S', 'A', 'G', 'U'),
            Array('B', 'V', 'K', 'P', 'W'),
            Array('X', 'M', 'Y', 'T', 'L')),
      Map('J' -> 'I'))
      
  private val squareWithWrongSize = PolybiusSquare(
      Array(Array('a', 'b', 'c', 'd', 'e'), 
            Array('f', 'g', 'h', 'k', 'l')))

  private val transpositionKey = "Battle"
  private val plaintext  = "Attack AT DAWN"
  private val ciphertext = "FFFFFFFFGFDDXGDAXDXGGXGX"
  private val nonStrictCipher = ADFGX(square, transpositionKey)
  private val strictCipher = ADFGX(square, transpositionKey, true)
  
  "WrongCiphertextException" must "have correct exception message" in {
    val ex = new WrongCiphertextCharException('a', 5, "ADFGX")
    ex.getMessage must be ("Ciphertext char 'a' at position 5 is not one of 'ADFGX' chars")
  }
  
  ".apply" must "throw an exception if Polybius square has size different from 5 x 5" in {
    val ex = the [WrongSquareSizeException] thrownBy ADFGX(squareWithWrongSize, transpositionKey)
    ex.getMessage must be ("Square must have a 5 x 5 size")
  }
  
  ".encrypt" must "correctly encrypt plaintext" in {
    val _ciphertext = nonStrictCipher.encrypt(plaintext)
    _ciphertext must be (ciphertext)
    
    nonStrictCipher.encrypt("") must be ("")
  }
  
  it must "throw an exception in strict mode if plaintext contains char that is missing in Polybius square" in {
    val ex = the [DataCharNotInSquareException] thrownBy strictCipher.encrypt(plaintext)
    ex.char must be (' ')
    ex.position must be (plaintext.indexOf(' '))
  }
  
  ".decrypt" must "correctly decrypt ciphertext" in {
    val _plaintext = nonStrictCipher.decrypt(ciphertext.toLowerCase)
    _plaintext must be ("ATTACKATDAWN")
    
    nonStrictCipher.decrypt("") must be ("")
  }
  
  it must "throw an exception if ciphertext length is odd" in {
    an [OddCiphertextLengthException] must be thrownBy nonStrictCipher.decrypt("A")
  }
  
  it must "throw an exception if ciphertext contains char that is not one of 'ADFGX' chars" in {
    val ex = the [WrongCiphertextCharException] thrownBy nonStrictCipher.decrypt("a1dfgx")
    ex.char must be ('1')
    ex.position must be (1)
  }
  
}
