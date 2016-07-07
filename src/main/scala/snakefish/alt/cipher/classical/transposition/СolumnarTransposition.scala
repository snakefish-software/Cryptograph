package snakefish.alt
package cipher.classical
package transposition

abstract class СolumnarTransposition(
  val key: CharSequence,
  val alphabet: Alphabet,
  val sameKeyLetterAsNext: Boolean) {
  
  protected val keyNums = normalizeKey(indicesInAlphabet(key, alphabet), sameKeyLetterAsNext)
  protected val colsCount = keyNums.length
  
  def colsToCiphertext(cols: Array[StringBuilder], ciphertext: StringBuilder): Unit
  
  def ciphertextToCols(ciphertext: CharSequence, cols: Array[StringBuilder]): Unit
  
  def encrypt(plaintext: CharSequence): String = {
    val ptLength = plaintext.length
    val cols = createEmptyCols(getRowsCount(ptLength))
    
    for (i <- 0 until ptLength) {
      cols(i % colsCount) += plaintext.charAt(i)
    }
    
    val ciphertext = new StringBuilder(ptLength)
    colsToCiphertext(cols, ciphertext)
    ciphertext.toString
  }
  
  def decrypt(ciphertext: CharSequence): String = {
    val ctLength = ciphertext.length
    val rowsCount = getRowsCount(ctLength)
    
    val cols = createEmptyCols(rowsCount)
    ciphertextToCols(ciphertext, cols)
    
    val plaintext = new StringBuilder(ctLength)
    for {
       rowIndex <- 0 until rowsCount
       colIndex <- 0 until colsCount
    } {
      val col = cols(colIndex)
      if (rowIndex < col.length)
        plaintext += col.charAt(rowIndex)
    }
    
    plaintext.toString
  }
  
  protected def getRowsCount(dataLength: Int): Int = 
    Math.ceil(dataLength.toDouble / colsCount).toInt
    
  protected def getLastRowLength(dataLength: Int): Int = 
    colsCount - (colsCount * getRowsCount(dataLength) - dataLength)
    
  protected def getColHeight(dataLength: Int, colIndex: Int): Int = {
    val rowsCount = getRowsCount(dataLength)
    val lastRowLength = getLastRowLength(dataLength)
    if (colIndex >= lastRowLength) rowsCount - 1 else rowsCount
  }
  
  private def createEmptyCols(colsHeight: Int): Array[StringBuilder] = {
    val cols = new Array[StringBuilder](colsCount)
    for (i <- 0 until colsCount) {
      cols(i) = new StringBuilder(colsHeight)
    }
    cols
  }
  
}
