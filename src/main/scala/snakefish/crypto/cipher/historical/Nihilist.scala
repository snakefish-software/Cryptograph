package snakefish.crypto
package cipher.historical

import PolybiusSquare._
import scala.collection.mutable.ArrayBuffer

object Nihilist {
  
  class CiferTextNumberException() extends Exception("Cifertext contains incorrect numbers")
  
  def encode(data: CharSequence, key: CharSequence, square: PolybiusSquare, strictMode: Boolean = false) = {
    val keyNumsOpt = toNums(key, square)
    if (keyNumsOpt.isEmpty) throw new KeyCharNotInSquareException()
    
    val keyNums = keyNumsOpt.get
    val result = new ArrayBuffer[Int](data.length)
    val rowColPair = new Array[Int](2)
    
    var keyNumInd = 0
    for (i <- 0 until data.length) {
      val dataCh = data.charAt(i)
      if (computeCoords(dataCh.toLower, square, rowColPair)) {
        val dataNum = toInt(rowColPair)
        val keyNum = keyNums(keyNumInd % keyNums.length)
        result += dataNum + keyNum
        keyNumInd += 1
      } else if (strictMode) {
        erase(keyNums)
        erase(rowColPair)
        erase(result)
        throw new DataCharNotInSquareException()
      }
    }
    
    erase(keyNums)
    erase(rowColPair)
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
        erase(keyNums)
        erase(result)
        throw new CiferTextNumberException()
      } else {
        val row = resultNum / 10 - 1
        val col = resultNum % 10 - 1
        result(i) = square(row)(col)
      }
    }
    
    erase(keyNums)
    result
  }
  
  private def toNums(chars: CharSequence, square: PolybiusSquare): Option[Array[Int]] = {
    val nums = new Array[Int](chars.length)
    val rowColPair = new Array[Int](2)
    for (i <- 0 until chars.length) {
      val ch = chars.charAt(i)
      if (computeCoords(ch.toLower, square, rowColPair)) {
        nums(i) = toInt(rowColPair)
      } else {
        erase(nums)
        erase(rowColPair)
        return None
      }
    }
    erase(rowColPair)
    Some(nums)
  }
  
  private def toInt(rowColPair: Array[Int]) = {
    val row = rowColPair(0) + 1
    val col = rowColPair(1) + 1
    row * 10 + col
  }
  
}
