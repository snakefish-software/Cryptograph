package snakefish.crypto
package cipher.classical
package transposition

abstract class СolumnarTransposition(key: CharSequence, 
                                     alphabet: Alphabet, 
                                     sameKeyLetterAsNext: Boolean) {
  
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
  
  private def normalizeKey(key: Array[Int], sameLetterAsNextInOrder: Boolean): Array[Int] = {
    val res = new Array[Int](key.length)
    var keySorted = key.clone.sorted
    if (!sameLetterAsNextInOrder)
      keySorted = keySorted.distinct
    
    for (i <- 0 until key.length) {
      val index = keySorted.indexOf(key(i))
      res(i) = index
      
      if (sameLetterAsNextInOrder) 
        keySorted(index) = -1
    }
    
    res
  }
  
  private def createEmptyCols(colsHeight: Int): Array[StringBuilder] = {
    val cols = new Array[StringBuilder](colsCount)
    for (i <- 0 until colsCount) {
      cols(i) = new StringBuilder(colsHeight)
    }
    cols
  }
  
}
