package snakefish.crypto
package cipher.classical

import PolybiusSquare._
import scala.collection.mutable.ArrayBuffer

object Nihilist {
  
  def apply(key: CharSequence, square: PolybiusSquare, strictMode: Boolean = false) = 
    new Nihilist(key, square, strictMode)
}

class Nihilist (val key: CharSequence, val square: PolybiusSquare, val strictMode: Boolean = false) {
  
  @throws(classOf[DataCharNotInSquareException])
  def encrypt(plaintext: CharSequence): List[Int] = {
    val keyNums = toNums(key)
    val keyLength = keyNums.length
      
    val ciphertext = new ArrayBuffer[Int](plaintext.length)
    var keyCharIndex = 0
    
    for (i <- 0 until plaintext.length) {
      val ptChar = plaintext.charAt(i)
      square.coords(ptChar) match {
        case Some((row, col)) =>
          val ptNum = toInt(row, col)
          val keyNum = if (keyLength > 0) keyNums(keyCharIndex % keyLength) else 0
          ciphertext += ptNum + keyNum
          keyCharIndex += 1
        
        case None =>
          if (strictMode) throw new DataCharNotInSquareException(ptChar, i)
      }
    }
    
    ciphertext.toList
  }

  @throws(classOf[CoordinatesOutOfBoundsException])
  def decrypt(ciphertext: Array[Int]): String = {
    val keyNums = toNums(key)
    val keyLength = keyNums.length
    
    val plaintext = new StringBuilder(ciphertext.length)
    val maxPossibleResNum = square.rowsCount * 10 + square.lastRowLength
    
    for (i <- 0 until ciphertext.length) {
      val keyNum = if (keyLength > 0) keyNums(i % keyLength) else 0
      val resultNum = ciphertext(i) - keyNum
      val row = resultNum / 10 - 1
      val col = resultNum % 10 - 1
      val rowCorrect = row >= 0 && row < square.rowsCount
      
      if (rowCorrect && col >= 0 && col < square(row).length) {
        plaintext += square(row)(col)
      } else if (strictMode) throw new CoordinatesOutOfBoundsException(i, row, col)
    }
    
    plaintext.toString
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
  
  @inline
  private def toInt(row: Int, col: Int) = (row + 1) * 10 + (col + 1)
  
}
