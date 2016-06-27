package snakefish.crypto
package cipher.classical

import PolybiusSquare._
import Playfair._

class PlayfairTest extends BaseTest {
  
  private val plaintext  = "Hello one and all"
  private val ciphertext = "kcnvmppoabocfqnv"
  private val notFilledLastRowSquare = PolybiusSquare(
    Array(Array('a', 'b', 'c', 'd'), 
          Array('e', 'f')))
          
  private val nonStrictCipher = Playfair(PolybiusSquare.LATIN, 'X')
  private val strictCipher = Playfair(PolybiusSquare.LATIN, 'X', true)
  
  ".apply" must "throw an exception if last row in Polybius square is not filled" in {
    an [WrongSquareSizeException] must be thrownBy Playfair(notFilledLastRowSquare, 'a')
  }
  
  it must "throw an exception if placeholder is missing in Polybius square" in {
    an [PlaceholderNotInSquareException] must be thrownBy Playfair(PolybiusSquare.LATIN, 'щ')
  }
  
  ".encrypt" must "correctly encrypt plaintext" in {
    val _ciphertext = nonStrictCipher.encrypt(plaintext)
    _ciphertext must be (ciphertext)
    
    nonStrictCipher.encrypt("") must be ("")
  }
  
  it must "throw an exception in strict mode if plaintext contains char that is missing in Polybius square" in {
    val ex = the [DataCharNotInSquareException] thrownBy strictCipher.encrypt(plaintext)
    ex.position must be (plaintext.indexOf(' '))
  }
  
  ".decrypt" must "correctly decrypt ciphertext" in {
    val _plaintext = nonStrictCipher.decrypt(ciphertext)
    _plaintext must be ("hellooneandallx")
    
    nonStrictCipher.decrypt("") must be ("")
  }
  
  it must "throw an exception in strict mode if ciphertext contains char that is missing in Polybius square" in {
    val ex = the [DataCharNotInSquareException] thrownBy strictCipher.decrypt(plaintext)
    ex.position must be (plaintext.indexOf(' '))
  }
  
  it must "throw an exception if ciphertext length is odd" in {
    an [OddCiphertextLengthException] must be thrownBy nonStrictCipher.decrypt("g")
  }
  
}
