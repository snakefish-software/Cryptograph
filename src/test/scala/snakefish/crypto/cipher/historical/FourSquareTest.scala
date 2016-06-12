package snakefish.crypto
package cipher.historical

import FourSquare._
import PolybiusSquare._

class FourSquareTest extends BaseTest {
  
  private val plaintext = "Hello stranger"
  private val cifertext = "ивййлспсвйибрф"
  
  private val ciferSquare = PolybiusSquare(
    Array(Array('а', 'б', 'в', 'г', 'д'),
          Array('е', 'ё', 'ж', 'з', 'и'),
          Array('й', 'к', 'л', 'м', 'н'),
          Array('о', 'п', 'р', 'с', 'т'),
          Array('у', 'ф', 'х', 'ц', 'ч')))                                     
                                           
  private val notFilledLastRowSquare = PolybiusSquare(
    Array(Array('а', 'б', 'в', 'г', 'д'),
          Array('е', 'ё', 'ж', 'з', 'и'),
          Array('й', 'к', 'л', 'м', 'н'),
          Array('о', 'п', 'р', 'с', 'т'),
          Array('у', 'ф', 'х', 'ц'))) 
          
  private val nonStrictCifer = FourSquare(LATIN, ciferSquare, ciferSquare)
  private val strictCifer = FourSquare(LATIN, ciferSquare, ciferSquare, true)
  
  ".apply" must "throw an exception if any 2 squares have different size" in {
    an [SquaresDifferentSizeException] must be thrownBy FourSquare(LATIN, RUSSIAN_SHORT, LATIN)
    an [SquaresDifferentSizeException] must be thrownBy FourSquare(LATIN, LATIN, RUSSIAN_SHORT)
  }
  
  ".apply" must "throw an exception if any square has not filled last row" in {
    an [WrongSquareSizeException] must be thrownBy FourSquare(notFilledLastRowSquare, ciferSquare, ciferSquare)
    an [WrongSquareSizeException] must be thrownBy FourSquare(ciferSquare, notFilledLastRowSquare, ciferSquare)
    an [WrongSquareSizeException] must be thrownBy FourSquare(ciferSquare, ciferSquare, notFilledLastRowSquare)
  }
  
  ".encode" must "correctly encode data using provided parameters" in {
    val _cifertext = nonStrictCifer.encode(plaintext, 'x')
    _cifertext must be (cifertext)
  }
  
  ".decode" must "correctly decode data using provided parameters" in {
    val _plaintext = nonStrictCifer.decode(cifertext)
    _plaintext must be ("hellostrangerx")
  }
  
  ".encode(strictMode)" must "throw an exception if data contains char that is missing in plain Polybius square" in {
    val ex = the [DataCharNotInSquareException] thrownBy strictCifer.encode(plaintext, 'x')
    ex.position must be (plaintext.indexOf(' '))
  }
  
  ".decode(strictMode)" must "throw an exception if data contains char that is missing in relevant cifer Polybius squares" in {
    val ex = the [DataCharNotInSquareException] thrownBy strictCifer.decode("абв гд")
    ex.position must be (3)
  }
  
  ".encode" must "throw an exception if placeholder is missing in Polybius square" in {
    an [PlaceholderNotInSquareException] must be thrownBy nonStrictCifer.encode(plaintext, 'щ')
  }
  
  ".decode" must "throw an exception if cifertext length is odd" in {
    an [OddCifertextLengthException] must be thrownBy nonStrictCifer.decode("и")
  }
  
}
