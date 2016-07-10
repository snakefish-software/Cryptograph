package snakefish.crypto
package cipher.classical

import ADFGX._
import PolybiusSquare._
import transposition.Columnar

object ADFGX {
  
  @throws(classOf[WrongSquareSizeException])
  def apply(square: PolybiusSquare, transpositionKey: CharSequence, strictMode: Boolean = false) = 
    new ADFGX(square, transpositionKey, strictMode)
  
  case class WrongCiphertextCharException(char: Char, position: Int, private val cipherChars: String) 
      extends RuntimeException(s"Ciphertext char '$char' at position $position is not one of '$cipherChars' chars")
}

class ADFGX (val square: PolybiusSquare, 
             val transpositionKey: CharSequence, 
             val strictMode: Boolean = false) {
  
  protected val cipherChars = "ADFGX"
  private val cipherCharsLen = cipherChars.length
  
  if (square.rowsCount != cipherCharsLen || square.colsCount != cipherCharsLen)
    throw new WrongSquareSizeException(s"Square must have a $cipherCharsLen x $cipherCharsLen size")
  
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
          if (strictMode) throw new DataCharNotInSquareException(ptChar, i)
      }
    }
    transposition.encrypt(ptInCipherChars)
  }
  
  @throws(classOf[OddCiphertextLengthException])
  @throws(classOf[WrongCiphertextCharException])
  def decrypt(ciphertext: CharSequence): String = {
    val ctLength = ciphertext.length
    
    if (ctLength % 2 == 1)
      throw new OddCiphertextLengthException()
    
    for (i <- 0 until ctLength) {
      val ctChar = ciphertext.charAt(i)
      if (!cipherChars.contains(ctChar.toUpper)) 
        throw new WrongCiphertextCharException(ctChar, i, cipherChars)
    }
    
    val plaintext = new StringBuilder(ctLength / 2)
    val ctTransposed = transposition.decrypt(ciphertext)
    for (i <- 0 until ctLength by 2) {
      val rowChar = ctTransposed.charAt(i)
      val row = cipherChars.indexOf(rowChar.toUpper)
      
      val colChar = ctTransposed.charAt(i + 1)
      val col = cipherChars.indexOf(colChar.toUpper)
      
      plaintext += square(row)(col)
    }
    
    plaintext.toString
  }

}
