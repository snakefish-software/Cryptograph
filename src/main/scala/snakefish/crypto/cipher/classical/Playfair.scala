package snakefish.crypto
package cipher.classical

import Playfair._
import PolybiusSquare._
import scala.collection.mutable.ArrayBuffer

object Playfair {

  @throws(classOf[WrongSquareSizeException])
  @throws(classOf[PlaceholderNotInSquareException])
  def apply(square: PolybiusSquare, placeholder: Char, strictMode: Boolean = false) =
    new Playfair(square, placeholder, strictMode)
}

class Playfair(val square: PolybiusSquare, private val _placeholder: Char, val strictMode: Boolean = false) {

  val placeholder = _placeholder.toLower

  if (!square.lastRowIsFilled)
    throw new WrongSquareSizeException("Last row of Polybius square is not filled")

  if (!square.contains(placeholder))
    throw new PlaceholderNotInSquareException()

  @throws(classOf[DataCharNotInSquareException])
  def encrypt(plaintext: CharSequence): String = {
    val inSquareChars = filter(plaintext, square, strictMode)
    val dataLength = inSquareChars.length
    val charsToComp = new StringBuilder(dataLength)

    var i = 0
    while (i < dataLength) {
      val ch1 = inSquareChars(i)
      charsToComp += ch1
      if (i + 1 < dataLength) {
        val ch2 = inSquareChars(i + 1)
        if (ch1 == ch2) {
          charsToComp += placeholder
        } else {
          charsToComp += ch2
          i += 1
        }
      } else {
        charsToComp += placeholder
      }
      i += 1
    }

    crypt(charsToComp, addByModulo).toString
  }

  @throws(classOf[OddCiphertextLengthException])
  @throws(classOf[DataCharNotInSquareException])
  def decrypt(ciphertext: CharSequence): String = {
    val charsToComp = filter(ciphertext, square, strictMode)

    if (strictMode && charsToComp.length % 2 == 1)
      throw new OddCiphertextLengthException()

    val decrypted = crypt(charsToComp, subtractByModulo)
    val plaintext = new StringBuilder(decrypted.length)

    for (i <- 0 until decrypted.length by 2) {
      val ch1 = decrypted(i)
      val ch2 = decrypted(i + 1)

      plaintext += ch1

      val isPlaceholder = ch2 == placeholder &&
                          i + 2 < decrypted.length &&
                          decrypted(i + 2) == ch1

      if (!isPlaceholder)
        plaintext += ch2
    }

    plaintext.toString
  }

  private def crypt(data: CharSequence, sameRowColFunc: (Int, Int, Int) => Int): StringBuilder = {
    val result = new StringBuilder(data.length)

    for (i <- 0 until data.length by 2) {
      if (i + 1 < data.length) {
        val Some((row1, col1)) = square.coords(data.charAt(i))
        val Some((row2, col2)) = square.coords(data.charAt(i + 1))

        if (row1 == row2) {
          val col1New = sameRowColFunc(col1, 1, square.colsCount)
          val col2New = sameRowColFunc(col2, 1, square.colsCount)
          result += square(row1)(col1New)
          result += square(row2)(col2New)
        } else if (col1 == col2) {
          val row1New = sameRowColFunc(row1, 1, square.rowsCount)
          val row2New = sameRowColFunc(row2, 1, square.rowsCount)
          result += square(row1New)(col1)
          result += square(row2New)(col2)
        } else {
          result += square(row1)(col2)
          result += square(row2)(col1)
        }
      }
    }

    result
  }

}
