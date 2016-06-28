package snakefish.crypto
package cipher.classical
package transposition

object Columnar {
  
  def apply(key: CharSequence, alphabet: Alphabet) = new Columnar(key, alphabet)
}

class Columnar(key: CharSequence, alphabet: Alphabet) {
  
  private val keyNums = normalizeKey(indicesInAlphabet(key, alphabet), true)
  
  def encrypt(plaintext: CharSequence): String = {
    val ptLength = plaintext.length
    val colsCount = keyNums.length
    val rowsCount = Math.ceil(ptLength.toDouble / colsCount).toInt
    
    val cols = createEmptyCols(colsCount, rowsCount)
    
    for (i <- 0 until ptLength) {
      cols(i % colsCount) += plaintext.charAt(i)
    }
    
    val result = new StringBuilder(ptLength)
    for (i <- 0 until colsCount) {
      result append cols(keyNums.indexOf(i))
    }
    
    result.toString
  }
  
  def decrypt(ciphertext: CharSequence): String = {
    val ctLength = ciphertext.length
    val colsCount = keyNums.length
    val rowsCount = Math.ceil(ctLength.toDouble / colsCount).toInt
    val lastRowLength = colsCount - (colsCount * rowsCount - ctLength)
    
    val cols = createEmptyCols(colsCount, rowsCount)
    
    var ctPos = 0
    for (colNum <- 0 until colsCount) {
      val colIndex = keyNums.indexOf(colNum)
      val colHeight = if (colIndex >= lastRowLength) rowsCount - 1 else rowsCount
      for (i <- 0 until colHeight) {
        cols(colIndex) += ciphertext.charAt(ctPos + i) 
      }
      ctPos += colHeight
    }
    
    val result = new StringBuilder(ctLength)
    for {
       row <- 0 until rowsCount - 1
       col <- 0 until colsCount
    } {
      result.append(cols(col).charAt(row))
    }
    
    for (col <- 0 until lastRowLength if rowsCount >= 1) {
      result.append(cols(col).charAt(rowsCount - 1))
    }
    
    result.toString
  }
  
  private def createEmptyCols(colsCount: Int, colsHeight: Int): Array[StringBuilder] = {
    val cols = new Array[StringBuilder](colsCount)
    for (i <- 0 until colsCount) {
      cols(i) = new StringBuilder(colsHeight)
    }
    cols
  }
  
}
