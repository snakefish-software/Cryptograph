package snakefish.crypto
package cipher.historical

import PolybiusSquare._
import scala.collection.mutable.ArrayBuffer

object Nihilist {
  
  case class CifertextNumberException(position: Int) 
      extends RuntimeException(s"Cifertext contains incorrect number at position $position")
  
  @throws(classOf[DataCharNotInSquareException])
  def encode(
    data: CharSequence,
    key: CharSequence,
    square: PolybiusSquare,
    strictMode: Boolean = false
  ): ArrayBuffer[Int] = {
    val keyNums = toNums(key, square)
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

  @throws(classOf[CifertextNumberException])
  def decode(
    data: Array[Int],
    key: CharSequence,
    square: PolybiusSquare,
    strictMode: Boolean = false
  ): ArrayBuffer[Char] = {
    val keyNums = toNums(key, square)
    val keyLength = keyNums.length
    
    val result = new ArrayBuffer[Char](data.length)
    val maxPossibleResNum = square.rowsCount * 10 + square.lastRowLength
    
    for (i <- 0 until data.length) {
      val keyNum = if (keyLength > 0) keyNums(i % keyLength) else 0
      val resultNum = data(i) - keyNum
      
      if (resultNum >= 11 && resultNum <= maxPossibleResNum) {
        val row = resultNum / 10 - 1
        val col = resultNum % 10 - 1
        if (row < square.rowsCount && col < square(row).length) {
          result += square(row)(col)
        } else if (strictMode) throw new CifertextNumberException(i)
      } else if (strictMode) throw new CifertextNumberException(i)
    }
    
    result
  }
  
  private def toNums(chars: CharSequence, square: PolybiusSquare): ArrayBuffer[Int] = {
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
