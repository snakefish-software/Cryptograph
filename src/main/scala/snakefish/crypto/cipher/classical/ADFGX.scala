package snakefish.crypto
package cipher.classical

import ADFGX._
import PolybiusSquare._
import transposition.Columnar

object ADFGX {
  
  @throws(classOf[WrongSquareSizeException])
  def apply(square: PolybiusSquare, transpositionKey: CharSequence, strictMode: Boolean = false) = 
    new ADFGX(square, transpositionKey, strictMode)
  
  case class WrongCiphertextException(position: Int, private val cipherChars: String) 
      extends RuntimeException(s"Ciphertext char at position $position is not one of '$cipherChars' chars")
}

class ADFGX(square: PolybiusSquare, transpositionKey: CharSequence, strictMode: Boolean = false) {
  
  protected val cipherChars = "ADFGX"
  private val cipherCharsLength = cipherChars.length
  
  if (square.rowsCount != cipherCharsLength || square.colsCount != cipherCharsLength)
    throw new WrongSquareSizeException(s"Square must have a $cipherCharsLength x $cipherCharsLength size")
  
  private val transposition = Columnar(transpositionKey, Alphabet.ENGLISH)
  
  @throws(classOf[DataCharNotInSquareException])
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
          if (strictMode) throw new DataCharNotInSquareException(i)
      }
    }
    transposition.encrypt(ptInCipherChars)
  }
  
  @throws(classOf[OddCiphertextLengthException])
  @throws(classOf[WrongCiphertextException])
  def decrypt(ciphertext: CharSequence): String = {
    val ctLength = ciphertext.length
    if (ctLength % 2 == 1)
      throw new OddCiphertextLengthException()
    
    for (i <- 0 until ctLength)
      if (!cipherChars.contains(ciphertext.charAt(i).toUpper))
        throw new WrongCiphertextException(i, cipherChars)
    
    val plaintext = new StringBuilder(ctLength / 2)
    val ctUntransposed = transposition.decrypt(ciphertext)
    for (i <- 0 until ctLength by 2) {
      val rowCh = ctUntransposed.charAt(i)
      val row = cipherChars.indexOf(rowCh.toUpper)
      
      val colCh = ctUntransposed.charAt(i + 1)
      val col = cipherChars.indexOf(colCh.toUpper)
      
      plaintext += square(row)(col)
    }
    
    plaintext.toString
  }

}
