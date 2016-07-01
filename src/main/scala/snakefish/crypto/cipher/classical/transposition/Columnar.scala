package snakefish.crypto
package cipher.classical
package transposition

object Columnar {
  
  def apply(key: CharSequence, alphabet: Alphabet) = 
    new Columnar(key, alphabet)
}

class Columnar(key: CharSequence, alphabet: Alphabet) 
  extends СolumnarTransposition(key, alphabet, true) {
  
  def colsToCiphertext(cols: Array[StringBuilder], ciphertext: StringBuilder): Unit = {
    for (i <- 0 until colsCount) 
      ciphertext append cols(keyNums.indexOf(i))
  }
  
  def ciphertextToCols(ciphertext: CharSequence, cols: Array[StringBuilder]): Unit = {
    var ctPos = 0
    for (colNum <- 0 until colsCount) {
      val colIndex = keyNums.indexOf(colNum)
      val colHeight = getColHeight(ciphertext.length, colIndex)
      for (i <- 0 until colHeight) {
        cols(colIndex) += ciphertext.charAt(ctPos + i) 
      }
      ctPos += colHeight
    }
  }
  
}
