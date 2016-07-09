package snakefish.crypto
package cipher.classical

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scala.language.implicitConversions

case class PolybiusSquare(square: Array[Array[Char]], missedToExisting: Map[Char, Char] = Map()) {
  
  private val charsToCoords = new HashMap[Char, (Int, Int)]()
  
  for {
    row <- 0 until square.length
    col <- 0 until square(row).length
  } {
    val charLower = square(row)(col).toLower
    charsToCoords.put(charLower, (row, col))
  }
  
  for ((missed, existing) <- missedToExisting) {
    val existingCoords = charsToCoords.get(existing.toLower)
    if (existingCoords.isDefined)
      charsToCoords.put(missed.toLower, existingCoords.get)
  }
  
  def apply(row: Int): Array[Char] = square(row)
  
  def rowsCount: Int = square.length
  def colsCount: Int = square(0).length
  
  def coords(ch: Char): Option[(Int, Int)] = charsToCoords.get(ch.toLower)
  def contains(ch: Char): Boolean = coords(ch).isDefined
  
  def lastRowLength: Int = square(square.length - 1).length
  def lastRowIsFilled: Boolean = lastRowLength == colsCount
  
}

object PolybiusSquare {
  
  case class KeyCharNotInSquareException(position: Int) 
      extends RuntimeException(s"Key char at position $position is missing in Polybius square")

  case class DataCharNotInSquareException(position: Int) 
      extends RuntimeException(s"Data char at position $position is missing in Polybius square")
  
  case class CoordinatesOutOfBoundsException(position: Int, row: Int, col: Int)
      extends RuntimeException(s"Coordinates (row = $row; column = $col) of char at position $position are out of Polybius square bounds")
  
  case class WrongSquareSizeException(msg: String)
      extends RuntimeException(msg)
  
  implicit def squareToArray(square: PolybiusSquare): Array[Array[Char]] = square.square
  
  implicit def arrayToSquare(array: Array[Array[Char]]): PolybiusSquare = PolybiusSquare(array)

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
  
  def apply(key: CharSequence, alphabet: Alphabet): PolybiusSquare = 
    apply(key, alphabet, Map[Char, Char]())

  def apply(key: CharSequence, alphabet: Alphabet, missedToExisting: Map[Char, Char]): PolybiusSquare = {
    val missedToExistingLower = missedToExisting map { case (k, v) => (k.toLower, v.toLower) }
    val squareChars = new ArrayBuffer[Char](alphabet.length)
    
    def tryToAddToSquare(ch: Char): Unit = {
      if (missedToExistingLower.contains(ch)) return
      if (squareChars.contains(ch)) return
      if (!alphabet.contains(ch)) return
      squareChars += ch
    }

    for (i <- 0 until key.length) {
      val keyChar = key.charAt(i).toLower
      tryToAddToSquare(keyChar)
    }
    
    alphabet.toString.foreach(tryToAddToSquare)
    
    PolybiusSquare(createSquare(squareChars), missedToExistingLower)
  }
  
  def apply(key: Long, alphabet: Alphabet): PolybiusSquare = 
    apply(key, alphabet, Map[Char, Char]())
  
  def apply(key: Long, alphabet: Alphabet, missedToExisting: Map[Char, Char]): PolybiusSquare = {
    val missedToExistingLower = missedToExisting map { case (k, v) => (k.toLower, v.toLower) }
    val squareChars = new StringBuilder(alphabet.length)
    
    alphabet.toString.foreach(ch => {
      if (!missedToExistingLower.contains(ch) && !squareChars.contains(ch))
        squareChars += ch
    })
    
    val shuffled = shuffle(key, squareChars)
    
    PolybiusSquare(createSquare(shuffled), missedToExistingLower)
  }
  
  private def createSquare(chars: CharSequence): Array[Array[Char]] = {
    val rowsCount = Math.sqrt(chars.length).toInt
    val colsCount = Math.ceil(chars.length.toDouble / rowsCount).toInt
    
    val square = Array.ofDim[Char](rowsCount, colsCount)
    for (i <- 0 until chars.length) {
      val row = i / colsCount
      val col = i % colsCount
      square(row)(col) = chars.charAt(i)
    }
    square
  }

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
      val dataChar = data.charAt(i)
      square.coords(dataChar) match {
        case Some((row, col)) =>
          dataNums += row
          dataNums += col
        
        case None =>
          if (strictMode) {
            throw new DataCharNotInSquareException(i)
          } else notInSquareChars.put(i, dataChar)
      }
    }
    
    val compDataNums = computeFunc(dataNums, square)
    
    val result = new StringBuilder(data.length)
    var inSquareIndex = 0
    for (i <- 0 until data.length) {
      val notInSquareChar = notInSquareChars.get(i)
      if (notInSquareChar.isEmpty) {
        val row = compDataNums(inSquareIndex * 2)
        val col = compDataNums(inSquareIndex * 2 + 1)
        
        if (row < square.rowsCount && col < square(row).length) {
          var resultChar = square(row)(col)
          if (data.charAt(i).isUpper) {
            resultChar = resultChar.toUpper
          }
          result += resultChar
          inSquareIndex += 1
        } else throw new CoordinatesOutOfBoundsException(i, row, col)
      } else result += notInSquareChar.get
    }
    result.toString
  }
  
  @throws(classOf[DataCharNotInSquareException])
  def filter(data: CharSequence, square: PolybiusSquare, strictMode: Boolean): StringBuilder = {
    val inSquareChars = new StringBuilder(data.length)
    
    for (i <- 0 until data.length) {
      val dataChar = data.charAt(i)
      if (square.contains(dataChar)) {
        inSquareChars.append(dataChar)
      } else if (strictMode) throw new DataCharNotInSquareException(i)
    }
    
    inSquareChars
  }

  def lowerSymbol(data: ArrayBuffer[Int], square: PolybiusSquare): Array[Int] = {
    val result = new Array[Int](data.length)
    for (i <- 0 until data.length by 2) {
      val row = data(i)
      val col = data(i + 1)

      var newRow = addByModulo(row, 1, square.rowsCount)
      if (col >= square(newRow).length)
        newRow = 0

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
      if (col >= square(newRow).length)
        newRow -= 1

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
