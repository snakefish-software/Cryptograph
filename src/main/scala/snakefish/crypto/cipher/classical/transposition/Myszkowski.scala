package snakefish.crypto
package cipher.classical
package transposition

import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer

object Myszkowski {
  
  def apply(key: CharSequence, alphabet: Alphabet) = 
    new Myszkowski(key, alphabet)
}

class Myszkowski(key: CharSequence, alphabet: Alphabet) 
  extends СolumnarTransposition(key, alphabet, false) {
  
  private val maxKeyNum = keyNums.max
  private val keyNumsToColsIndices = new HashMap[Int, ArrayBuffer[Int]]()
  
  for (colIndex <- 0 until colsCount) {
    val keyNum = keyNums(colIndex)
    val colsIndices = keyNumsToColsIndices.getOrElseUpdate(keyNum, new ArrayBuffer[Int](2))
    colsIndices += colIndex
  }
  
  def colsToCiphertext(cols: Array[StringBuilder], ciphertext: StringBuilder): Unit = {
    for (keyNum <- 0 to maxKeyNum) {
      val Some(colsIndices) = keyNumsToColsIndices.get(keyNum)
      val rowsCount = cols(0).length
      for {
        rowIndex <- 0 until rowsCount
        colIndex <- colsIndices
      } {
        val col = cols(colIndex)
        if (rowIndex < col.length)
          ciphertext += col.charAt(rowIndex)
      }
    }
  }
  
  def ciphertextToCols(ciphertext: CharSequence, cols: Array[StringBuilder]): Unit = {
    var ctPos = 0
    for (keyNum <- 0 to maxKeyNum) {
      val Some(colsIndices) = keyNumsToColsIndices.get(keyNum)
      var totalColsSize = 0
      for (colIndex <- colsIndices) {
        totalColsSize += getColHeight(ciphertext.length, colIndex)
      }
      for (i <- 0 until totalColsSize) {
        val colIndex = colsIndices(i % colsIndices.length)
        cols(colIndex) += ciphertext.charAt(ctPos + i)
      }
      ctPos += totalColsSize
    }
  }
  
}
