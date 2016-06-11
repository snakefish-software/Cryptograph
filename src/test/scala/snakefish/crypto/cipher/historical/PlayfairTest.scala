package snakefish.crypto
package cipher.historical

import PolybiusSquare._
import Playfair._

class PlayfairTest extends BaseTest {
  
  private val plaintext = "Hello one and all"
  private val cifertext = "kcnvmppoabocfqnv"
  private val notFilledLastRowSquare = PolybiusSquare(
    Array(Array('a', 'b', 'c', 'd'), 
          Array('e', 'f', 'g', 'h'),
          Array('i', 'k', 'l', 'm'),
          Array('n', 'o', 'p', 'q'),
          Array('r', 's', 't', 'u'),
          Array('v', 'w', 'x', 'y'),
          Array('z', 'j')))
          
  private val nonStrictCifer = Playfair(PolybiusSquare.LATIN, 'x')
  private val strictCifer = Playfair(PolybiusSquare.LATIN, 'x', true)
  
  ".encode" must "correctly encode data using provided parameters" in {
    val _cifertext = nonStrictCifer.encode(plaintext)
    _cifertext must be (cifertext.toCharArray)
  }
  
  ".decode" must("correctly decode data using provided parameters") in {
    val _plaintext = nonStrictCifer.decode(cifertext)
    _plaintext must be ("helxlooneandallx".toCharArray)
  }
  
  ".encode(strictMode)" must "throw an exception if data contains char that is missing in Polybius square" in {
    val ex = the [DataCharNotInSquareException] thrownBy strictCifer.encode(plaintext)
    ex.position must be (plaintext.indexOf(' '))
  }
  
  ".decode(strictMode)" must "throw an exception if data contains char that is missing in Polybius square" in {
    val ex = the [DataCharNotInSquareException] thrownBy strictCifer.decode(plaintext)
    ex.position must be (plaintext.indexOf(' '))
  }
  
  ".apply" must "throw an exception if last row in Polybius square is not filled" in {
    an [WrongSquareSizeException] must be thrownBy Playfair(notFilledLastRowSquare, 'x')
  }
  
  ".decode" must "throw an exception if cifertext length is odd" in {
    an [OddCifertextLengthException] must be thrownBy (nonStrictCifer.decode("g"))
  }
  
}
