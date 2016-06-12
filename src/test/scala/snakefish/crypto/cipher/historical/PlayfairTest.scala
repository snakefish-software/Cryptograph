package snakefish.crypto
package cipher.historical

import PolybiusSquare._
import Playfair._

class PlayfairTest extends BaseTest {
  
  private val plaintext = "Hello one and all"
  private val cifertext = "kcnvmppoabocfqnv"
  private val notFilledLastRowSquare = PolybiusSquare(
    Array(Array('a', 'b', 'c', 'd'), 
          Array('e', 'f')))
          
  private val nonStrictCifer = Playfair(PolybiusSquare.LATIN)
  private val strictCifer = Playfair(PolybiusSquare.LATIN, true)
  
  ".apply" must "throw an exception if last row in Polybius square is not filled" in {
    an [WrongSquareSizeException] must be thrownBy Playfair(notFilledLastRowSquare)
  }
  
  ".encode" must "correctly encode data using provided parameters" in {
    val _cifertext = nonStrictCifer.encode(plaintext, 'x')
    _cifertext must be (cifertext)
  }
  
  ".decode" must "correctly decode data using provided parameters" in {
    val _plaintext = nonStrictCifer.decode(cifertext)
    _plaintext must be ("helxlooneandallx")
  }
  
  ".encode(strictMode)" must "throw an exception if data contains char that is missing in Polybius square" in {
    val ex = the [DataCharNotInSquareException] thrownBy strictCifer.encode(plaintext, 'x')
    ex.position must be (plaintext.indexOf(' '))
  }
  
  ".decode(strictMode)" must "throw an exception if data contains char that is missing in Polybius square" in {
    val ex = the [DataCharNotInSquareException] thrownBy strictCifer.decode(plaintext)
    ex.position must be (plaintext.indexOf(' '))
  }
  
  ".encode" must "throw an exception if placeholder is missing in Polybius square" in {
    an [PlaceholderNotInSquareException] must be thrownBy nonStrictCifer.encode(plaintext, 'щ')
  }
  
  ".decode" must "throw an exception if cifertext length is odd" in {
    an [OddCifertextLengthException] must be thrownBy nonStrictCifer.decode("g")
  }
  
}
