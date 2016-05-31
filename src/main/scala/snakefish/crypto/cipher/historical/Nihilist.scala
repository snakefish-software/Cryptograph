package snakefish.crypto
package cipher.historical

import PolybiusSquare._
import scala.collection.mutable.ArrayBuffer

object Nihilist {
  
  class CifertextNumberException() extends Exception("Cifertext contains incorrect numbers")
  
  def encode(data: CharSequence, key: CharSequence, square: PolybiusSquare, strictMode: Boolean = false) = {
    val keyNumsOpt = toNums(key, square)
    if (keyNumsOpt.isEmpty) throw new KeyCharNotInSquareException()
    
    val keyNums = keyNumsOpt.get
    val result = new ArrayBuffer[Int](data.length)
    var keyNumInd = 0
    
    for (i <- 0 until data.length) {
      square.coords(data.charAt(i)) match {
        case Some((row, col)) => {
          val dataNum = toInt(row, col)
          val keyNum = keyNums(keyNumInd % keyNums.length)
          result += dataNum + keyNum
          keyNumInd += 1
        }
        
        case None => {
          if (strictMode) throw new DataCharNotInSquareException()
        }
      }
    }
    
    result
  }

  def decode(data: Array[Int], key: CharSequence, square: PolybiusSquare) = {
    val keyNumsOpt = toNums(key, square)
    if (keyNumsOpt.isEmpty) throw new KeyCharNotInSquareException()
    
    val keyNums = keyNumsOpt.get
    val result = new Array[Char](data.length)
    val maxPossibleResNum = square.rowsCount * 10 + square.lastRowLength
    
    for (i <- 0 until data.length) {
      val keyNum = keyNums(i % keyNums.length)
      val resultNum = data(i) - keyNum
      if (resultNum < 11 || resultNum > maxPossibleResNum) {
        throw new CifertextNumberException()
      } else {
        val row = resultNum / 10 - 1
        val col = resultNum % 10 - 1
        result(i) = square(row)(col)
      }
    }
    
    result
  }
  
  private def toNums(chars: CharSequence, square: PolybiusSquare): Option[Array[Int]] = {
    val nums = new Array[Int](chars.length)
    for (i <- 0 until chars.length) {
      square.coords(chars.charAt(i)) match {
        case Some((row, col)) => nums(i) = toInt(row, col)
        case None => return None
      }
    }
    Some(nums)
  }
  
  private def toInt(row: Int, col: Int) = (row + 1) * 10 + (col + 1)
  
}