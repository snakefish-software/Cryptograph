package snakefish.crypto
package cipher.historical

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

case class PolybiusSquare(val square: Array[Array[Char]], val missedOnExisting: Map[Char, Char] = Map()) {
  
  private val _coords = new Array[Int](2)
  
  def apply(row: Int) = square(row)
  def rowsCount = square.length
  def colsCount = square(0).length
  def lastRowLength = square(square.length - 1).length
  
  def coords(ch: Char): Option[Array[Int]] = {
    val chLower = ch.toLower
    for (row <- 0 until rowsCount) {
      for (col <- 0 until square(row).length) {
        if (square(row)(col) == chLower) {
          _coords(0) = row
          _coords(1) = col
          return Some(_coords)
        }
      }
    }

    val mappingCh = missedOnExisting.get(chLower)
    if (mappingCh.isEmpty) None else coords(mappingCh.get)
  }
  
  def erase(): Unit = snakefish.crypto.erase(_coords)
}

object PolybiusSquare {
  
  class KeyCharNotInSquareException() extends Exception("Key contains symbols that are missing in provided square")

  class DataCharNotInSquareException() extends Exception("Data contains symbols that are missing in provided square")
  
  implicit def squareToArray(square: PolybiusSquare): Array[Array[Char]] = square.square
  
  implicit def arrayToSquare(array: Array[Array[Char]]) : PolybiusSquare = PolybiusSquare(array)

  val LATIN = PolybiusSquare(Array(Array('a', 'b', 'c', 'd', 'e'),
                                   Array('f', 'g', 'h', 'i', 'k'),
                                   Array('l', 'm', 'n', 'o', 'p'),
                                   Array('q', 'r', 's', 't', 'u'),
                                   Array('v', 'w', 'x', 'y', 'z')),
                             Map('j' -> 'i'))

  val RUSSIAN_ALL = PolybiusSquare(Array(Array('а', 'б', 'в', 'г', 'д', 'е'),
                                         Array('ё', 'ж', 'з', 'и', 'й', 'к'),
                                         Array('л', 'м', 'н', 'о', 'п', 'р'),
                                         Array('с', 'т', 'у', 'ф', 'х', 'ц'),
                                         Array('ч', 'ш', 'щ', 'ъ', 'ы', 'ь'),
                                         Array('э', 'ю', 'я')))

  val RUSSIAN_SHORT = PolybiusSquare(Array(Array('а', 'б', 'в', 'г', 'д', 'е'),
                                           Array('ж', 'з', 'и', 'к', 'л', 'м'),
                                           Array('н', 'о', 'п', 'р', 'с', 'т'),
                                           Array('у', 'ф', 'х', 'ц', 'ч', 'ш'),
                                           Array('щ', 'ы', 'ь', 'э', 'ю', 'я')),
                                     Map('ё' -> 'е',
                                         'й' -> 'и',
                                         'ъ' -> 'ь'))
  
  def apply(key: CharSequence, alphabet: String): PolybiusSquare = 
    apply(key, alphabet, Map[Char, Char]())

  def apply(key: CharSequence, alphabet: String, missedOnExisting: Map[Char, Char]): PolybiusSquare = {
    val alphabetL = alphabet.toLowerCase
    val missedOnExistingL = missedOnExisting map { case (k, v) => (k.toLower, v.toLower) }
    val sqChars = new ArrayBuffer[Char](alphabetL.length)
    
    def tryToAddToSquare(ch: Char): Unit = {
      if (missedOnExistingL.contains(ch)) return
      if (sqChars.contains(ch)) return
      if (!alphabetL.contains(ch)) return
      sqChars += ch
    }

    for (i <- 0 until key.length) {
      val keyCh = key.charAt(i).toLower
      tryToAddToSquare(keyCh)
    }
    
    alphabetL.foreach(tryToAddToSquare)
    
    val square = createSquare(sqChars)
    erase(sqChars)
    
    PolybiusSquare(square, missedOnExistingL)
  }
  
  def apply(key: Long, alphabet: String): PolybiusSquare = 
    apply(key, alphabet, Map[Char, Char]())
  
  def apply(key: Long, alphabet: String, missedOnExisting: Map[Char, Char]): PolybiusSquare = {
    val alphabetL = alphabet.toLowerCase
    val missedOnExistingL = missedOnExisting map { case (k, v) => (k.toLower, v.toLower) }
    val sqChars = new StringBuilder(alphabetL.length)
    
    alphabetL.foreach((ch: Char) => {
      if (!missedOnExistingL.contains(ch) && !sqChars.contains(ch))
        sqChars += ch
    })
    
    val shuffled = shuffle(sqChars, key)
    val square = createSquare(shuffled)
    erase(shuffled)
    
    PolybiusSquare(square, missedOnExistingL)
  }
  
  private def createSquare(chars: CharSequence) = {
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

  def compute(data: CharSequence, square: PolybiusSquare, computeFunc: (ArrayBuffer[Int], Array[Array[Char]]) => Array[Int], strictMode: Boolean = false) = {
    val dataNums = new ArrayBuffer[Int](data.length * 2)
    val notInSquareChars = new mutable.HashMap[Int, Char]()

    for (i <- 0 until data.length) {
      val dataCh = data.charAt(i)
      
      square.coords(dataCh) match {
        case Some(coords) => {
          dataNums += coords(0)
          dataNums += coords(1)
        }
        
        case None => {
          if (strictMode) {
            erase(dataNums)
            throw new DataCharNotInSquareException()
          } else notInSquareChars.put(i, dataCh)
        }
      }
    }

    val compDataNums = computeFunc(dataNums, square)
    erase(dataNums)

    val result = new Array[Char](data.length)
    var inSqInd = 0
    for (i <- 0 until data.length) {
      val notInSquareCh = notInSquareChars.get(i)
      if (notInSquareCh.isEmpty) {
        val row = compDataNums(inSqInd * 2)
        val col = compDataNums(inSqInd * 2 + 1)
        if (row >= square.rowsCount || col >= square(row).length) {
          erase(compDataNums)
          erase(result)
          throw new DataCharNotInSquareException()
        } else {
          var resCh = square(row)(col)
          if (data.charAt(i).isUpper) {
            resCh = resCh.toUpper
          }
          result(i) = resCh
          inSqInd += 1
        }
      } else result(i) = notInSquareCh.get
    }

    erase(compDataNums)
    result
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
