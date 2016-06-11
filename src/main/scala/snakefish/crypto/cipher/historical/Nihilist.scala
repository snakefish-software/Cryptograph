package snakefish.crypto
package cipher.historical

import PolybiusSquare._
import scala.collection.mutable.ArrayBuffer

object Nihilist {
  def apply(square: PolybiusSquare, strictMode: Boolean = false) = 
    new Nihilist(square, strictMode)
}

class Nihilist(val square: PolybiusSquare, val strictMode: Boolean = false) {
  
  @throws(classOf[DataCharNotInSquareException])
  def encode(data: CharSequence, key: CharSequence): ArrayBuffer[Int] = {
    val keyNums = toNums(key)
    val keyLength = keyNums.length
      
    val result = new ArrayBuffer[Int](data.length)
    var keyChIndex = 0
    
    for (i <- 0 until data.length) {
      square.coords(data.charAt(i)) match {
        case Some((row, col)) =>
          val dataNum = toInt(row, col)
          val keyNum = if (keyLength > 0) keyNums(keyChIndex % keyLength) else 0
          result += dataNum + keyNum
          keyChIndex += 1
        
        case None =>
          if (strictMode) throw new DataCharNotInSquareException(i)
      }
    }
    
    result
  }

  @throws(classOf[CoordinatesOutOfBoundsException])
  def decode(data: Array[Int], key: CharSequence): ArrayBuffer[Char] = {
    val keyNums = toNums(key)
    val keyLength = keyNums.length
    
    val result = new ArrayBuffer[Char](data.length)
    val maxPossibleResNum = square.rowsCount * 10 + square.lastRowLength
    
    for (i <- 0 until data.length) {
      val keyNum = if (keyLength > 0) keyNums(i % keyLength) else 0
      val resultNum = data(i) - keyNum
      val row = resultNum / 10 - 1
      val col = resultNum % 10 - 1
      val rowCorrect = row >= 0 && row < square.rowsCount
      
      if (rowCorrect && col >= 0 && col < square(row).length) {
        result += square(row)(col)
      } else throw new CoordinatesOutOfBoundsException(i, row, col)
    }
    
    result
  }
  
  private def toNums(chars: CharSequence): ArrayBuffer[Int] = {
    val nums = new ArrayBuffer[Int](chars.length)
    for (i <- 0 until chars.length) {
      square.coords(chars.charAt(i)).foreach { 
        case (row, col) => nums += toInt(row, col)
      }
    }
    nums
  }
  
  private def toInt(row: Int, col: Int) = (row + 1) * 10 + (col + 1)
  
}
