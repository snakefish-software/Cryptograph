package snakefish.alt
package cipher.classical

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scala.language.implicitConversions
import PolybiusSquare.Coord

sealed trait PolybiusSquare {
  def row(row: Int): Array[Char]
  def charAt(row: Int, col: Int): Option[Char]
  def contains(ch: Char): Boolean
  def rowsCount: Int
  def colsCount: Int
  def coords(ch: Char): Option[Coord]
  def lastRowLength: Int
  def lastRowFilled: Boolean
}

object PolybiusSquare {

  type Coord = (Int, Int) // (row, col)

  private class PolybiusSquareImpl(
    square: Array[Array[Char]],
    coordinates: Map[Char, Coord]
  ) extends PolybiusSquare {

    def charAt(row: Int, col: Int) = opt(square, row).flatMap(opt(_, col))
    def row(row: Int) = square(row)
    def contains(x: Char) = coordinates.contains(x.toLower)
    def rowsCount = square.length
    def colsCount = square(0).length
    def coords(x: Char): Option[Coord] = coordinates.get(x.toLower)
    def lastRowLength = square(square.length - 1).length
    def lastRowFilled = lastRowLength == colsCount

    private def opt[T](x: Array[T], i: Int): Option[T] = {
      if (x.length < (i + 1)) None
      else Option(x(i))
    }
  }

  def apply(
    square: Array[Array[Char]],
    substitutions: Map[Char, Char] = Map(),
    strict: Boolean = false
  ): PolybiusSquare = {

    val coords = new HashMap[Char, Coord]()

    for {
      row <- 0 until square.length
      col <- 0 until square(row).length
    } coords.put(square(row)(col).toLower, (row, col))

    for {
      (missing, existing) <- substitutions
      coord <- coords.get(existing.toLower)
    } coords.put(missing.toLower, coord)

    new PolybiusSquareImpl(square, coords.toMap)
  }

  def apply(key: CharSequence, alphabet: Alphabet): PolybiusSquare =
    apply(key, alphabet, Map[Char, Char]())

  def apply(key: CharSequence, alphabet: Alphabet, substitutions: Map[Char, Char]): PolybiusSquare = {
    val lowerSubstitutions = substitutions map { case (k, v) => (k.toLower, v.toLower) }
    val squareChars = new ArrayBuffer[Char](alphabet.length)

    def tryToAddToSquare(ch: Char): Unit = {
      if (lowerSubstitutions.contains(ch)) return
      if (squareChars.contains(ch)) return
      if (!alphabet.contains(ch)) return
        squareChars += ch
    }

    for (i <- 0 until key.length) {
      val keyCh = key.charAt(i).toLower
      tryToAddToSquare(keyCh)
    }

    alphabet.toString.foreach(tryToAddToSquare)

    PolybiusSquare(makeSquareFrom(squareChars), lowerSubstitutions)
  }

  def apply(key: Long, alphabet: Alphabet): PolybiusSquare =
    apply(key, alphabet, Map[Char, Char]())

  def apply(key: Long, alphabet: Alphabet, substitutions: Map[Char, Char]): PolybiusSquare = {
    val subs = substitutions map { case (k, v) => (k.toLower, v.toLower) }
    val squareChars = new StringBuilder(alphabet.length)

    alphabet.chars.foreach(ch =>
      if (!subs.contains(ch) && !squareChars.contains(ch)) squareChars += ch
    )

    PolybiusSquare(makeSquareFrom(shuffle(key, squareChars)), subs)
  }

  private def makeSquareFrom(chars: CharSequence): Array[Array[Char]] = {
    val rows = Math.sqrt(chars.length).toInt
    val cols = Math.ceil(chars.length.toDouble / rows).toInt

    val square = Array.ofDim[Char](rows, cols)
    for (i <- 0 until chars.length) {
      val row = i / cols
      val col = i % cols
      square(row)(col) = chars.charAt(i)
    }
    square
  }

  case class KeyCharNotInSquareException(position: Int)
      extends RuntimeException(s"Key char at position $position is missing in Polybius square")

  case class DataCharNotInSquareException(position: Int)
      extends RuntimeException(s"Data char at position $position is missing in Polybius square")

  case class CoordinatesOutOfBoundsException(position: Int, row: Int, col: Int)
      extends RuntimeException(s"Coordinates (row = $row; column = $col) of char at position $position are out of Polybius square bounds")

  case class WrongSquareSizeException(msg: String)
      extends RuntimeException(msg)

  val LATIN = PolybiusSquare(
    Array(Array('a', 'b', 'c', 'd', 'e'),
      Array('f', 'g', 'h', 'i', 'k'),
      Array('l', 'm', 'n', 'o', 'p'),
      Array('q', 'r', 's', 't', 'u'),
      Array('v', 'w', 'x', 'y', 'z')),
    Map('j' -> 'i'))

  val RUSSIAN_ALL = PolybiusSquare(
    Array(Array('а', 'б', 'в', 'г', 'д', 'е'),
      Array('ё', 'ж', 'з', 'и', 'й', 'к'),
      Array('л', 'м', 'н', 'о', 'п', 'р'),
      Array('с', 'т', 'у', 'ф', 'х', 'ц'),
      Array('ч', 'ш', 'щ', 'ъ', 'ы', 'ь'),
      Array('э', 'ю', 'я')))

  val RUSSIAN_SHORT = PolybiusSquare(
    Array(Array('а', 'б', 'в', 'г', 'д', 'е'),
      Array('ж', 'з', 'и', 'к', 'л', 'м'),
      Array('н', 'о', 'п', 'р', 'с', 'т'),
      Array('у', 'ф', 'х', 'ц', 'ч', 'ш'),
      Array('щ', 'ы', 'ь', 'э', 'ю', 'я')),
    Map('ё' -> 'е',
      'й' -> 'и',
      'ъ' -> 'ь'))

  @throws(classOf[DataCharNotInSquareException])
  @throws(classOf[CoordinatesOutOfBoundsException])
  def compute(
    data: CharSequence,
    square: PolybiusSquare,
    computeFunc: (ArrayBuffer[Int], PolybiusSquare) => Array[Int],
    strictMode: Boolean = false
  ): String = {
    val dataNums = new ArrayBuffer[Int](data.length * 2)
    val notInSquareChars = new HashMap[Int, Char]()

    for (i <- 0 until data.length) {
      val dataCh = data.charAt(i)
      square.coords(dataCh) match {
        case Some((row, col)) =>
          dataNums += row
          dataNums += col

        case None =>
          if (strictMode) {
            throw new DataCharNotInSquareException(i)
          } else notInSquareChars.put(i, dataCh)
      }
    }

    val compDataNums = computeFunc(dataNums, square)

    val result = new StringBuilder(data.length)
    var inSquareIndex = 0
    for (i <- 0 until data.length) {
      val notInSquareCh = notInSquareChars.get(i)
      if (notInSquareCh.isEmpty) {
        val row = compDataNums(inSquareIndex * 2)
        val col = compDataNums(inSquareIndex * 2 + 1)

        if (row < square.rowsCount && col < square.row(row).length) {
          square.charAt(row, col).foreach { x =>
            if (data.charAt(i).isUpper) result += x.toUpper
            else result += x
            inSquareIndex += 1
          }
        } else throw new CoordinatesOutOfBoundsException(i, row, col)
      } else result += notInSquareCh.get
    }
    result.toString
  }

  @throws(classOf[DataCharNotInSquareException])
  def filter(data: CharSequence, square: PolybiusSquare, strictMode: Boolean): StringBuilder = {
    val inSquareChars = new StringBuilder(data.length)

    for (i <- 0 until data.length) {
      val ch = data.charAt(i)
      if (square.contains(ch))
        inSquareChars.append(ch)
      else if (strictMode) throw new DataCharNotInSquareException(i)
    }

    inSquareChars
  }

  def lowerSymbol(data: ArrayBuffer[Int], square: PolybiusSquare): Array[Int] = {
    val result = new Array[Int](data.length)
    for (i <- 0 until data.length by 2) {
      val row = data(i)
      val col = data(i + 1)

      var newRow = addByModulo(row, 1, square.rowsCount)
      if (col >= square.row(newRow).length) {
        newRow = 0
      }
      result(i) = newRow
      result(i + 1) = col
    }
    result
  }

  def upperSymbol(data: ArrayBuffer[Int], square: PolybiusSquare): Array[Int] = {
    val result = new Array[Int](data.length)
    for (i <- 0 until data.length by 2) {
      val row = data(i)
      val col = data(i + 1)

      var newRow = subtractByModulo(row, 1, square.rowsCount)
      if (col >= square.row(newRow).length) {
        newRow -= 1
      }

      result(i) = newRow
      result(i + 1) = col
    }
    result
  }

  def rowsCols(data: ArrayBuffer[Int], square: PolybiusSquare): Array[Int] = {
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

  def rowsColsReverse(data: ArrayBuffer[Int], square: PolybiusSquare): Array[Int] = {
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

  def colsRows(data: ArrayBuffer[Int], square: PolybiusSquare): Array[Int] = {
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

  def colsRowsReverse(data: ArrayBuffer[Int], square: PolybiusSquare): Array[Int] = {
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
