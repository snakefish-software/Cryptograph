package snakefish.crypto
package cipher.historical

import PolybiusSquare._
import Playfair._

class PlayfairTest extends BaseTest {
  
  private val plaintext  = "Hello one and all"
  private val ciphertext = "kcnvmppoabocfqnv"
  private val notFilledLastRowSquare = PolybiusSquare(
    Array(Array('a', 'b', 'c', 'd'), 
          Array('e', 'f')))
          
  private val nonStrictCipher = Playfair(PolybiusSquare.LATIN)
  private val strictCipher = Playfair(PolybiusSquare.LATIN, true)
  
  ".apply" must "throw an exception if last row in Polybius square is not filled" in {
    an [WrongSquareSizeException] must be thrownBy Playfair(notFilledLastRowSquare)
  }
  
  ".encrypt" must "correctly encrypt plaintext using provided parameters" in {
    val _ciphertext = nonStrictCipher.encrypt(plaintext, 'x')
    _ciphertext must be (ciphertext)
  }
  
  ".decrypt" must "correctly decrypt ciphertext using provided parameters" in {
    val _plaintext = nonStrictCipher.decrypt(ciphertext)
    _plaintext must be ("helxlooneandallx")
  }
  
  ".encrypt(strictMode)" must "throw an exception if plaintext contains char that is missing in Polybius square" in {
    val ex = the [DataCharNotInSquareException] thrownBy strictCipher.encrypt(plaintext, 'x')
    ex.position must be (plaintext.indexOf(' '))
  }
  
  ".decrypt(strictMode)" must "throw an exception if ciphertext contains char that is missing in Polybius square" in {
    val ex = the [DataCharNotInSquareException] thrownBy strictCipher.decrypt(plaintext)
    ex.position must be (plaintext.indexOf(' '))
  }
  
  ".encrypt" must "throw an exception if placeholder is missing in Polybius square" in {
    an [PlaceholderNotInSquareException] must be thrownBy nonStrictCipher.encrypt(plaintext, 'щ')
  }
  
  ".decrypt" must "throw an exception if ciphertext length is odd" in {
    an [OddCiphertextLengthException] must be thrownBy nonStrictCipher.decrypt("g")
  }
  
}
