package snakefish.crypto
package cipher.historical

import PolybiusSquare._
import FourSquare._
import scala.collection.mutable.ArrayBuffer

object FourSquare {
  
  @throws(classOf[SquaresDifferentSizeException])
  @throws(classOf[WrongSquareSizeException])
  def apply(plainSquare: PolybiusSquare, 
            ciferSquare1: PolybiusSquare, 
            ciferSquare2: PolybiusSquare,
            strictMode: Boolean = false
  ): FourSquare = {
    new FourSquare(plainSquare, 
                   ciferSquare1, 
                   ciferSquare2, 
                   strictMode)
  }
  
  case class SquaresDifferentSizeException()
      extends RuntimeException("Some squares have different size")
  
}

class FourSquare(val plainSquare: PolybiusSquare, 
                 val ciferSquare1: PolybiusSquare, 
                 val ciferSquare2: PolybiusSquare, 
                 val strictMode: Boolean = false) {
  
  if (plainSquare.rowsCount != ciferSquare1.rowsCount || 
      plainSquare.rowsCount != ciferSquare2.rowsCount || 
      plainSquare.colsCount != ciferSquare1.colsCount || 
      plainSquare.colsCount != ciferSquare2.colsCount)
    throw new SquaresDifferentSizeException()
  
  if (!plainSquare.lastRowFilled || !ciferSquare1.lastRowFilled || !ciferSquare2.lastRowFilled)
    throw new WrongSquareSizeException("Last row of square is not filled")
  
  @throws(classOf[PlaceholderNotInSquareException])
  @throws(classOf[DataCharNotInSquareException])
  def encode(data: CharSequence, placeholder: Char): String = {
    if (!plainSquare.contains(placeholder))
      throw new PlaceholderNotInSquareException()
    
    val inSquareChars = filter(data, plainSquare, strictMode)
    if (inSquareChars.length % 2 == 1) {
      inSquareChars.append(placeholder)
    }
      
    val result = new StringBuilder(inSquareChars.length)
    for (i <- 0 until inSquareChars.length by 2) {
      val (row1, col1) = plainSquare.coords(inSquareChars.charAt(i)).get
      val (row2, col2) = plainSquare.coords(inSquareChars.charAt(i + 1)).get
      result += ciferSquare1(row1)(col2)
      result += ciferSquare2(row2)(col1)
    }
    result.toString
  }
  
  @throws(classOf[OddCifertextLengthException])
  @throws(classOf[DataCharNotInSquareException])
  def decode(data: CharSequence): String = {
    val inSquareChars = new ArrayBuffer[Char](data.length)
    
    var inSqIndex = 0;
    for (i <- 0 until data.length) {
      val ch = data.charAt(i)
      val isInSquare = (inSqIndex % 2 == 0 && ciferSquare1.contains(ch)) ||
                       (inSqIndex % 2 == 1 && ciferSquare2.contains(ch))
      if (isInSquare) {
        inSquareChars += ch
        inSqIndex += 1
      } else if (strictMode) throw new DataCharNotInSquareException(i)
    }
    
    if (inSquareChars.length % 2 != 0)
      throw new OddCifertextLengthException()
    
    val result = new StringBuilder(inSquareChars.length)
    for (i <- 0 until inSquareChars.length by 2) {
      val (row1, col2) = ciferSquare1.coords(inSquareChars.charAt(i)).get
      val (row2, col1) = ciferSquare2.coords(inSquareChars.charAt(i + 1)).get
      result += plainSquare(row1)(col1)
      result += plainSquare(row2)(col2)
    }
    result.toString
  }
  
}