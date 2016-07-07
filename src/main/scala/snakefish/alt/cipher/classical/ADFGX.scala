package snakefish.alt
package cipher.classical

import PolybiusSquare._
import transposition.Columnar

class ADFGX private [classical](
  val square: PolybiusSquare,
  val transpositionKey: CharSequence,
  val strictMode: Boolean = false) {

  protected val cipherChars = "ADFGX"
  private val cipherCharsLen = cipherChars.length
  
  private val transposition = Columnar(transpositionKey, Alphabet.ENGLISH)
  
  def encrypt(plaintext: CharSequence): String = {
    val ptLength = plaintext.length
    val ptInCipherChars = new StringBuilder(2 * ptLength)
    for (i <- 0 until ptLength) {
      val ptChar = plaintext.charAt(i)
      square.coords(ptChar) match {
        case Some((row, col)) =>
          ptInCipherChars += cipherChars.charAt(row)
          ptInCipherChars += cipherChars.charAt(col)
        case None =>
          if (strictMode) throw new IllegalArgumentException("Data Char Not In Square Exception")
      }
    }
    transposition.encrypt(ptInCipherChars)
  }
  
  def decrypt(ciphertext: CharSequence): String = {
    val ctLength = {
      if (ciphertext.length % 2 == 1) ciphertext.length - 1
      else ciphertext.length
    }
    
    for (i <- 0 until ctLength)
      if (!cipherChars.contains(ciphertext.charAt(i).toUpper))
        throw new IllegalArgumentException(s"Wrong cipher test char at $i")

    val plaintext = new StringBuilder(ctLength / 2)
    val ctTransposed = transposition.decrypt(ciphertext)
    for (i <- 0 until ctLength by 2) {
      val rowCh = ctTransposed.charAt(i)
      val row = cipherChars.indexOf(rowCh.toUpper)
      
      val colCh = ctTransposed.charAt(i + 1)
      val col = cipherChars.indexOf(colCh.toUpper)
      
      plaintext += square(row)(col)
    }
    
    plaintext.toString
  }

}

object ADFGX {
  def apply(square: PolybiusSquare, transpositionKey: CharSequence, strictMode: Boolean = false) = {
    require(square.rowsCount == 5 && square.colsCount == 5, "Square must have a 5x5 size")
    new ADFGX(square, transpositionKey, strictMode)
  }
}
