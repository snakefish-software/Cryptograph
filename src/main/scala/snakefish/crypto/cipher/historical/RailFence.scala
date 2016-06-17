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
    val ptLength = plaintext.length
    val result = new StringBuilder(ptLength)
    
    val peakPeriod = 2 * rowsCount - 2
    val peaks = 0 until ptLength by peakPeriod
    
    // First row
    for (peak <- peaks)
      result += plaintext.charAt(peak)
    
    // Rows between first and last
    for (rows <- 1 until rowsCount - 1) {
      for (peak <- peaks) {
        val leftIndex = peak - rows
        val rightIndex = peak + rows
        if (leftIndex >= 0)
          result += plaintext.charAt(leftIndex)
        if (rightIndex < ptLength)
          result += plaintext.charAt(rightIndex)
      }
    }
    
    // Last row
    for (btm <- (peakPeriod / 2) until ptLength by peakPeriod)
      result += plaintext.charAt(btm)
      
    result.toString
  }
  
  def decrypt(ciphertext: CharSequence): String = {
    val ctLength = ciphertext.length
    val result = new Array[Char](ctLength)
    var ctIndex = 0
    
    val peakPeriod = 2 * rowsCount - 2
    val peaks = 0 until ctLength by peakPeriod
    
    // First row
    for (peak <- peaks) {
      result(peak) = ciphertext.charAt(ctIndex)
      ctIndex += 1
    }
    
    // Rows between first and last
    for (row <- 1 until rowsCount - 1) {
      for (peak <- peaks) {
        val leftIndex = peak - row
        val rightIndex = peak + row
        if (leftIndex >= 0) {
          result(leftIndex) = ciphertext.charAt(ctIndex)
          ctIndex += 1
        }
        if (rightIndex < ctLength) {
          result(rightIndex) = ciphertext.charAt(ctIndex)
          ctIndex += 1
        }
      }
    }
    
    // Last row
    for (btm <- (peakPeriod / 2) until ctLength by peakPeriod) {
      result(btm) = ciphertext.charAt(ctIndex)
      ctIndex += 1
    }
    
    result.mkString("")
  }
  
}
