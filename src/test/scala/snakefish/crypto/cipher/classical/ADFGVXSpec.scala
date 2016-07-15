package snakefish.crypto
package cipher.classical

import ADFGX._
import PolybiusSquare._

class ADFGVXSpec extends BaseSpec {
  
  private val square = PolybiusSquare(
    Array(Array('1', 'G', 'R', '4', 'H', 'D'),
          Array('E', '2', 'A', 'V', '9', 'M'),
          Array('8', 'P', 'I', 'N', 'K', 'Z'),
          Array('B', 'Y', 'U', 'F', '6', 'T'),
          Array('5', 'G', 'X', 'S', '3', 'O'),
          Array('W', 'L', 'Q', '7', 'C', '0')))
         
  private val squareWithWrongSize = PolybiusSquare(
    Array(Array('a', 'b', 'c', 'd', 'e', 'q'), 
          Array('f', 'g', 'h', 'k', 'l', 'z')))
          
  private val transpositionKey = "Secret"
  private val plaintext  = "Attack will begin in 11 AM"
  private val ciphertext = "GXFGFFDFFADDFAGFXDFADXVFAFGFDDXXVFAXVDAGAX"
  private val ciphertextWithNonDecrChars = " GXFGFFDFFADDFAGF XDFADXVFAFGFDDXXVFAXVDAGAX "
  private val nonStrictCipher = ADFGVX(square, transpositionKey)
  private val strictCipher = ADFGVX(square, transpositionKey, true)

  ".apply" must "throw an exception if Polybius square has size different from 6 x 6" in {
    val ex = the [WrongSquareSizeException] thrownBy ADFGVX(squareWithWrongSize, transpositionKey)
    ex.getMessage must be ("Square must have a 6 x 6 size")
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
    val _plaintext1 = nonStrictCipher.decrypt(ciphertext.toLowerCase)
    _plaintext1 must be ("ATTACKWILLBEGININ11AM")
    
    val _plaintext2 = nonStrictCipher.decrypt(ciphertextWithNonDecrChars)
    _plaintext2 must be ("ATTACKWILLBEGININ11AM")
    
    nonStrictCipher.decrypt("") must be ("")
  }
  
  it must "throw an exception in strict mode if ciphertext length is odd" in {
    an [OddCiphertextLengthException] must be thrownBy strictCipher.decrypt("A")
  }
  
  it must "throw an exception in strict mode if ciphertext contains char that is not one of 'ADFGVX' chars" in {
    val ex = the [WrongCiphertextCharException] thrownBy strictCipher.decrypt("a dfgx")
    ex.char must be (' ')
    ex.position must be (1)
  }
  
}
