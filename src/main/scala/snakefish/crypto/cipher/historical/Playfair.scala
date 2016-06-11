package snakefish.crypto
package cipher.historical

import PolybiusSquare._
import scala.collection.mutable.ArrayBuffer

object Playfair {
  
  class OddCifertextLengthException
      extends RuntimeException("Cifertext length must be even")
  
  @throws(classOf[WrongSquareSizeException])
  @throws(classOf[DataCharNotInSquareException])
  def encode(
    data: CharSequence, 
    square: PolybiusSquare, 
    placeholder: Char, 
    strictMode: Boolean = false
  ): Array[Char] = {
    val inSquareChars = filter(data, square, strictMode)
    val dataLen = inSquareChars.length
    val charsToComp = new StringBuilder(dataLen)
    
    var i = 0
    while (i < dataLen) {
      val ch1 = inSquareChars.charAt(i)
      charsToComp.append(ch1)
      if (i + 1 < dataLen) {
        val ch2 = inSquareChars.charAt(i + 1)
        if (ch1 == ch2) {
          charsToComp.append(placeholder)
        } else {
          charsToComp.append(ch2)
          i += 1
        }
      } else {
        charsToComp.append(placeholder)
      }
      i += 1
    }
    
    compute(charsToComp, square)(addByModulo)
  }
  
  @throws(classOf[WrongSquareSizeException])
  @throws(classOf[OddCifertextLengthException])
  @throws(classOf[DataCharNotInSquareException])
  def decode(
    data: CharSequence, 
    square: PolybiusSquare,
    strictMode: Boolean = false
  ): Array[Char] = {
    val charsToComp = filter(data, square, strictMode)
    
    if (charsToComp.length % 2 != 0)
      throw new OddCifertextLengthException()
    
    compute(charsToComp, square)(subtractByModulo)
  }
  
  private def compute(
    data: CharSequence, 
    square: PolybiusSquare
  )( 
    sameRowColFunc: (Int, Int, Int) => Int
  ): Array[Char] = {
    if (square.lastRowLength != square.colsCount)
      throw new WrongSquareSizeException("Last row of Polybius square is not filled")
    
    val result = new Array[Char](data.length)
    
    for (i <- 0 until data.length by 2) {
      val (row1, col1) = square.coords(data.charAt(i)).get
      val (row2, col2) = square.coords(data.charAt(i + 1)).get
      
      if (row1 == row2) {
        val col1New = sameRowColFunc(col1, 1, square.colsCount)
        val col2New = sameRowColFunc(col2, 1, square.colsCount)
        result(i) = square(row1)(col1New)
        result(i + 1) = square(row2)(col2New)
      } else if (col1 == col2) {
        val row1New = sameRowColFunc(row1, 1, square.rowsCount)
        val row2New = sameRowColFunc(row2, 1, square.rowsCount)
        result(i) = square(row1New)(col1)
        result(i + 1) = square(row2New)(col2)
      } else {
        result(i) = square(row1)(col2)
        result(i + 1) = square(row2)(col1)
      }
    }
    
    result
  }
  
}
