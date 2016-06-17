package snakefish.crypto.cipher.historical

import RailFence._

object RailFence {
  
  @throws(classOf[IllegalRowsCountException])
  def apply(rowsCount: Int) = new RailFence(rowsCount)
  
  case class IllegalRowsCountException()
      extends RuntimeException("Rows count must be > 1")
  
}

class RailFence(val rowsCount: Int) {
  
  if (rowsCount < 2)
    throw new IllegalRowsCountException()
  
  def encrypt(plaintext: CharSequence): String = {
    val ptLen = plaintext.length
    val result = new StringBuilder(ptLen)
    
    val peakPeriod = 2 * rowsCount - 2
    val peaks = 0 until ptLen by peakPeriod
    
    for (peak <- peaks)
      result += plaintext.charAt(peak)
      
    for (rows <- 1 until rowsCount - 1) {
      for (peak <- peaks) {
        val leftIndex = peak - rows
        val rightIndex = peak + rows
        if (leftIndex >= 0)
          result += plaintext.charAt(leftIndex)
        if (rightIndex < ptLen)
          result += plaintext.charAt(rightIndex)
      }
    }
    
    for (btm <- (peakPeriod / 2) until ptLen by peakPeriod)
      result += plaintext.charAt(btm)
      
    result.toString
  }
  
  def decrypt(ciphertext: CharSequence): String = {
    val ciphertextLen = ciphertext.length
    val result = new Array[Char](ciphertextLen)
    var ctIndex = 0
    
    val peakPeriod = 2 * rowsCount - 2
    val peaks = 0 until ciphertextLen by peakPeriod
    
    for (peak <- peaks) {
      result(peak) = ciphertext.charAt(ctIndex)
      ctIndex += 1
    }
    
    for (row <- 1 until rowsCount - 1) {
      for (peak <- peaks) {
        val leftIndex = peak - row
        val rightIndex = peak + row
        if (leftIndex >= 0) {
          result(leftIndex) = ciphertext.charAt(ctIndex)
          ctIndex += 1
        }
        if (rightIndex < ciphertextLen) {
          result(rightIndex) = ciphertext.charAt(ctIndex)
          ctIndex += 1
        }
      }
    }
    
    for (btm <- (peakPeriod / 2) until ciphertextLen by peakPeriod) {
      result(btm) = ciphertext.charAt(ctIndex)
      ctIndex += 1
    }
    
    result.mkString("")
  }
  
}
