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
  
  ".encode" must "correctly encode data using provided parameters" in {
    val _cifertext = Playfair.encode(plaintext, PolybiusSquare.LATIN, 'x')
    _cifertext must be (cifertext.toCharArray)
  }
  
  ".decode" must("correctly decode data using provided parameters") in {
    val _plaintext = Playfair.decode(cifertext, PolybiusSquare.LATIN)
    _plaintext must be ("helxlooneandallx".toCharArray)
  }
  
  ".encode(strictMode)" must "throw an exception if data contains char that is missing in Polybius square" in {
    val ex = the [DataCharNotInSquareException] thrownBy Playfair.encode(plaintext, PolybiusSquare.LATIN, 'x', true)
    ex.position must be (plaintext.indexOf(' '))
  }
  
  ".decode(strictMode)" must "throw an exception if data contains char that is missing in Polybius square" in {
    val ex = the [DataCharNotInSquareException] thrownBy Playfair.decode(plaintext, PolybiusSquare.LATIN, true)
    ex.position must be (plaintext.indexOf(' '))
  }
  
  ".encode" must "throw an exception if last row in Polybius square is not filled" in {
    an [WrongSquareSizeException] must be thrownBy (Playfair.encode(plaintext, notFilledLastRowSquare, 'x'))
  }
  
  ".decode" must "throw an exception if last row in Polybius square is not filled" in {
    an [WrongSquareSizeException] must be thrownBy (Playfair.decode(cifertext, notFilledLastRowSquare))
  }
  
  ".decode" must "throw an exception if cifertext length is odd" in {
    an [OddCifertextLengthException] must be thrownBy (Playfair.decode("g", PolybiusSquare.LATIN))
  }
  
}
