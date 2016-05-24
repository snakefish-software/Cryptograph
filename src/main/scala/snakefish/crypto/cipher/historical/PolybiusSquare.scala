package snakefish.crypto
package cipher.historical

import utils.MathOps._
import utils.CryptoUtils._
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object PolybiusSquare {

  class DataCharNotInSquareException() extends Exception("Data contains symbols that are missing in provided square")

  case class Square(val square: Array[Array[Char]], val missedOnExisting: Map[Char, Char] = Map())

  val LATIN = Square(Array(Array('a', 'b', 'c', 'd', 'e'),
                           Array('f', 'g', 'h', 'i', 'k'),
                           Array('l', 'm', 'n', 'o', 'p'),
                           Array('q', 'r', 's', 't', 'u'),
                           Array('v', 'w', 'x', 'y', 'z')),
                     Map('j' -> 'i'))

  val RUSSIAN_ALL = Square(Array(Array('а', 'б', 'в', 'г', 'д', 'е'),
                                 Array('ё', 'ж', 'з', 'и', 'й', 'к'),
                                 Array('л', 'м', 'н', 'о', 'п', 'р'),
                                 Array('с', 'т', 'у', 'ф', 'х', 'ц'),
                                 Array('ч', 'ш', 'щ', 'ъ', 'ы', 'ь'),
                                 Array('э', 'ю', 'я')))

  val RUSSIAN_SHORT = Square(Array(Array('а', 'б', 'в', 'г', 'д', 'е'),
                                   Array('ж', 'з', 'и', 'к', 'л', 'м'),
                                   Array('н', 'о', 'п', 'р', 'с', 'т'),
                                   Array('у', 'ф', 'х', 'ц', 'ч', 'ш'),
                                   Array('щ', 'ы', 'ь', 'э', 'ю', 'я')),
                             Map('ё' -> 'е',
                                 'й' -> 'и',
                                 'ъ' -> 'ь'))

  def compute(data: CharSequence, square: Square, computeFunc: (ArrayBuffer[Int], Array[Array[Char]]) => Array[Int], strictMode: Boolean = false) = {
    val dataNums = new ArrayBuffer[Int](data.length * 2)
    val rowColPair = new Array[Int](2)
    val notInSquareChars = new mutable.HashMap[Int, Char]()
    
    for (i <- 0 until data.length) {
      val dataCh = data.charAt(i)
      
      if (computeCoords(Character.toLowerCase(dataCh), square, rowColPair)) {
        dataNums += rowColPair(0)
        dataNums += rowColPair(1)
      } else {
        if (strictMode) {
          erase(dataNums)
          throw new DataCharNotInSquareException()
        } else notInSquareChars.put(i, dataCh)
      }
    }

    val sq = square.square
    val compDataNums = computeFunc(dataNums, sq)
    erase(dataNums)

    val result = new Array[Char](data.length)
    var inSqInd = 0
    for (i <- 0 until data.length) {
      val notInSquareCh = notInSquareChars.get(i)
      if (notInSquareCh.isEmpty) {
        val row = compDataNums(inSqInd * 2)
        val col = compDataNums(inSqInd * 2 + 1)
        if (row >= sq.length || col >= sq(row).length) {
          erase(compDataNums)
          erase(result)
          throw new DataCharNotInSquareException()
        } else {
          var resCh = sq(row)(col)
          if (Character.isUpperCase(data.charAt(i))) {
            resCh = Character.toUpperCase(resCh)
          }
          result(i) = resCh
          inSqInd += 1
        }
      } else result(i) = notInSquareCh.get
    }

    erase(compDataNums)
    result
  }

  private def computeCoords(ch: Char, square: Square, rowColPair: Array[Int]): Boolean = {
    val sq = square.square
    for (row <- 0 until sq.length) {
      for (col <- 0 until sq(row).length) {
        if (sq(row)(col) == ch) {
          rowColPair(0) = row
          rowColPair(1) = col
          return true
        }
      }
    }

    val mappingCh = square.missedOnExisting.get(ch)
    if (mappingCh.isEmpty) false else computeCoords(mappingCh.get, square, rowColPair)
  }

  def lowerSymbol(data: ArrayBuffer[Int], square: Array[Array[Char]]) = {
    val result = new Array[Int](data.length)
    for (i <- 0 until data.length by 2) {
      val row = data(i)
      val col = data(i + 1)

      var newRow = addByModulo(row, 1, square.length)
      if (col >= square(newRow).length)
        newRow = 0

      result(i) = newRow
      result(i + 1) = col
    }
    result
  }

  def upperSymbol(data: ArrayBuffer[Int], square: Array[Array[Char]]) = {
    val result = new Array[Int](data.length)
    for (i <- 0 until data.length by 2) {
      val row = data(i)
      val col = data(i + 1)

      var newRow = subtractByModulo(row, 1, square.length)
      if (col >= square(newRow).length)
        newRow -= 1

      result(i) = newRow
      result(i + 1) = col
    }
    result
  }

  def rowsCols(data: ArrayBuffer[Int], square: Array[Array[Char]]) = {
    val result = new Array[Int](data.length)
    val middle = data.length / 2
    for (i <- 0 until middle) {
      val row = data(i * 2)
      val col = data(i * 2 + 1)
      result(i) = row
      result(middle + i) = col
    }
    result
  }

  def rowsColsReverse(data: ArrayBuffer[Int], square: Array[Array[Char]]) = {
    val result = new Array[Int](data.length)
    val middle = data.length / 2
    for (i <- 0 until middle) {
      val row = data(i)
      val col = data(middle + i)
      result(i * 2) = row
      result(i * 2 + 1) = col
    }
    result
  }

  def colsRows(data: ArrayBuffer[Int], square: Array[Array[Char]]) = {
    val result = new Array[Int](data.length)
    val middle = data.length / 2
    for (i <- 0 until middle) {
      val row = data(i * 2)
      val col = data(i * 2 + 1)
      result(i) = col
      result(middle + i) = row
    }
    result
  }

  def colsRowsReverse(data: ArrayBuffer[Int], square: Array[Array[Char]]) = {
    val result = new Array[Int](data.length)
    val middle = data.length / 2
    for (i <- 0 until middle) {
      val col = data(i)
      val row = data(middle + i)
      result(i * 2) = row
      result(i * 2 + 1) = col
    }
    result
  }

}
