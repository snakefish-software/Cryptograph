package snakefish.crypto
package cipher.historical

import PolybiusSquare._
import scala.collection.mutable.ArrayBuffer

object Nihilist {
  
  class CifertextNumberException() extends Exception("Cifertext contains incorrect numbers")
  
  def encode(data: CharSequence, key: CharSequence, square: PolybiusSquare): ArrayBuffer[Int] = {
    val keyNums = toNums(key, square)
    val keyLength = keyNums.length
      
    val result = new ArrayBuffer[Int](data.length)
    var keyNumInd = 0
    
    for (i <- 0 until data.length) {
      square.coords(data.charAt(i)).foreach { 
        case (row, col) => 
          val dataNum = toInt(row, col)
          val keyNum = if (keyLength > 0) keyNums(keyNumInd % keyLength) else 0
          result += dataNum + keyNum
          keyNumInd += 1
      }
    }
    
    result
  }

  @throws(classOf[CifertextNumberException])
  def decode(data: Array[Int], key: CharSequence, square: PolybiusSquare): Array[Char] = {
    val keyNums = toNums(key, square)
    val keyLength = keyNums.length
    
    val result = new Array[Char](data.length)
    val maxPossibleResNum = square.rowsCount * 10 + square.lastRowLength
    
    for (i <- 0 until data.length) {
      val keyNum = if (keyLength > 0) keyNums(i % keyLength) else 0
      val resultNum = data(i) - keyNum
      
      if (resultNum < 11 || resultNum > maxPossibleResNum)
        throw new CifertextNumberException()
      
      val row = resultNum / 10 - 1
      val col = resultNum % 10 - 1
      result(i) = square(row)(col)
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
