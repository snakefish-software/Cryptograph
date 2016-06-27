package snakefish.crypto
package cipher.classical

import scala.collection.mutable.Map
import scala.collection.mutable.ListMap
import scala.collection.mutable.ArrayBuffer

object Adfgx extends AdfgvxCipher {
  val cipherChars = "ADFGX"
}

object Adfgvx extends AdfgvxCipher {
  val cipherChars = "ADFGVX"
}

trait AdfgvxCipher {

  def cipherChars: String

  def encrypt(data: String, key: String, square: PolybiusSquare): String = {
    val indexedKey = key.zipWithIndex
    val polibiusMap = Map[Char, String]()

    for(i <- 0 until square.colsCount; j <- 0 until square.rowsCount) {
      polibiusMap += square(i)(j) -> s"${cipherChars.charAt(i)}${cipherChars.charAt(j)}"
    }
    square.missedToExisting.foreach { case (k, v) =>
      polibiusMap.get(v).foreach { s =>
        polibiusMap += k -> s
      }
    }

    val substitutions = new StringBuilder()
    for (i <- 0 until data.length) {
      polibiusMap.get(data.charAt(i)).foreach { x =>
        substitutions.append(x)
      }
    }
    val split = splitAt(substitutions.toString, key.length)
    val transposed = new Array[StringBuffer](key.length)
    indexedKey.foreach { case (ch, i) =>
      val arr = Option(transposed(i)).getOrElse(new StringBuffer())
      for (j <- 0 until split.length) {
        if (i < split(j).length) {
          transposed(i) = arr append split(j)(i)
        }
      }
    }

    indexedKey
      .sortBy(_._1)
      .map(x => transposed(x._2).toString)
      .mkString
  }

  def decrypt(cipherText: String, key: String, square: PolybiusSquare): String = {
    val polibiusMap = Map[(Char, Char), Char]()

    for(i <- 0 until square.colsCount; j <- 0 until square.rowsCount) {
      polibiusMap += (cipherChars.charAt(i), cipherChars.charAt(j)) -> square(i)(j)
    }

    val transpositionMap =
      key.zipWithIndex
        .sortBy(_._1)
        .zipWithIndex
        .map{ case ((_, column), transposition) => transposition -> column}
        .toMap
    val cipherTextLength = cipherText.length
    val keyLength = key.length
    val rowCount = {
      val fullRows = cipherTextLength / keyLength
      if (fullRows * keyLength < cipherTextLength) fullRows + 1
      else fullRows
    }
    val lastRowLength =  keyLength - (rowCount * keyLength - cipherTextLength)
    val columns = new Array[String](keyLength)

    var tmp = cipherText
    for (i <- 0 until keyLength) {
      val transposedIndex = transpositionMap(i)
      val columnLength = {
        if (transposedIndex >= lastRowLength) rowCount - 1
        else rowCount
      }
      val (column, rest) = tmp.splitAt(columnLength)
      columns(transposedIndex) = column
      tmp = rest
    }

    val rowsText = new StringBuffer()
    for (i <- 0 until rowCount - 1) {
      for (j <- 0 until keyLength) {
        rowsText.append(columns(j).charAt(i))
      }
    }
    for (i <- 0 until lastRowLength) {
      rowsText.append(columns(i).charAt(rowCount - 1))
    }

    val result = new StringBuffer()
    for (i <- 0 until rowsText.length by 2) {
      result append polibiusMap(rowsText.charAt(i) -> rowsText.charAt(i + 1))
    }

    result.toString
  }

  def encode(data: String, key: String, square: PolybiusSquare): String =
    encrypt(data, key, square)

  def decode(data: String, key: String, square: PolybiusSquare): String =
    decrypt(data, key, square)

}
