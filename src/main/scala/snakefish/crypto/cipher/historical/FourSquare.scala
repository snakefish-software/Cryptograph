package snakefish.crypto
package cipher.historical

import PolybiusSquare._
import FourSquare._
import scala.collection.mutable.ArrayBuffer

object FourSquare {
  
  @throws(classOf[SquaresDifferentSizeException])
  @throws(classOf[WrongSquareSizeException])
  @throws(classOf[PlaceholderNotInSquareException])
  def apply(plainSquare: PolybiusSquare, 
            cipherSquare1: PolybiusSquare, 
            cipherSquare2: PolybiusSquare,
            placeholder: Char,
            strictMode: Boolean = false
  ): FourSquare = {
    new FourSquare(plainSquare, 
                   cipherSquare1, 
                   cipherSquare2,
                   placeholder,
                   strictMode)
  }
  
  case class SquaresDifferentSizeException()
      extends RuntimeException("Some squares have different size")
  
}

class FourSquare(val plainSquare: PolybiusSquare, 
                 val cipherSquare1: PolybiusSquare, 
                 val cipherSquare2: PolybiusSquare, 
                 private val _placeholder: Char, 
                 val strictMode: Boolean = false) {
  
  val placeholder = _placeholder.toLower
  
  if (plainSquare.rowsCount != cipherSquare1.rowsCount || 
      plainSquare.rowsCount != cipherSquare2.rowsCount || 
      plainSquare.colsCount != cipherSquare1.colsCount || 
      plainSquare.colsCount != cipherSquare2.colsCount)
    throw new SquaresDifferentSizeException()
  
  if (!plainSquare.lastRowFilled || !cipherSquare1.lastRowFilled || !cipherSquare2.lastRowFilled)
    throw new WrongSquareSizeException("Last row of square is not filled")
  
  if (!plainSquare.contains(placeholder))
    throw new PlaceholderNotInSquareException()
  
  @throws(classOf[DataCharNotInSquareException])
  def encrypt(plaintext: CharSequence): String = {
    val inSquareChars = filter(plaintext, plainSquare, strictMode)
    if (inSquareChars.length % 2 == 1) {
      inSquareChars.append(placeholder)
    }
      
    val result = new StringBuilder(inSquareChars.length)
    for (i <- 0 until inSquareChars.length by 2) {
      val (row1, col1) = plainSquare.coords(inSquareChars(i)).get
      val (row2, col2) = plainSquare.coords(inSquareChars(i + 1)).get
      result += cipherSquare1(row1)(col2)
      result += cipherSquare2(row2)(col1)
    }
    result.toString
  }
  
  @throws(classOf[OddCiphertextLengthException])
  @throws(classOf[DataCharNotInSquareException])
  def decrypt(ciphertext: CharSequence): String = {
    val inSquareChars = new ArrayBuffer[Char](ciphertext.length)
    
    var inSqIndex = 0;
    for (i <- 0 until ciphertext.length) {
      val ch = ciphertext.charAt(i)
      val isInSquare = (inSqIndex % 2 == 0 && cipherSquare1.contains(ch)) ||
                       (inSqIndex % 2 == 1 && cipherSquare2.contains(ch))
      if (isInSquare) {
        inSquareChars += ch
        inSqIndex += 1
      } else if (strictMode) throw new DataCharNotInSquareException(i)
    }
    
    if (inSquareChars.length % 2 != 0)
      throw new OddCiphertextLengthException()
    
    var result = new StringBuilder(inSquareChars.length)
    for (i <- 0 until inSquareChars.length by 2) {
      val (row1, col2) = cipherSquare1.coords(inSquareChars(i)).get
      val (row2, col1) = cipherSquare2.coords(inSquareChars(i + 1)).get
      result += plainSquare(row1)(col1)
      result += plainSquare(row2)(col2)
    }
    
    if (result.endsWith(placeholder.toString))
      result = result.dropRight(1)
      
    result.toString
  }
  
}
