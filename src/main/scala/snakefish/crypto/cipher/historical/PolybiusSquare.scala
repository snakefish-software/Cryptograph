package snakefish.crypto
package cipher.historical

import utils.MathOps._
import utils.CryptoUtils._
import scala.util.Try

object PolybiusSquare {
  
  class DataCharNotInSquareException() extends Exception("Data contains symbols that are missing in provided square")
  
  case class Square(val square: Array[Array[Char]], val missedOnExisting: Map[Char, Char] = Map())
  
  val LATIN =  Square(Array(Array('a',	'b', 'c',	'd', 'e'),
                            Array('f',	'g', 'h',	'i', 'k'),
                            Array('l',	'm', 'n',	'o', 'p'),
                            Array('q',	'r', 's',	't', 'u'),
                            Array('v',	'w', 'x',	'y', 'z')),
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
                                        
  def compute(data: CharSequence, square: Square, computeFunc: (Array[Byte], Array[Array[Char]]) => Array[Byte]) = {
    val dataNums = new Array[Byte](data.length * 2)
    val rowColPair = new Array[Byte](2)
    for (i <- 0 until data.length) {
      val dataCh = Character.toLowerCase(data.charAt(i))
      
      val computedCoords = computeCoords(dataCh, square, rowColPair)
      if (computedCoords.isEmpty) {
        eraseArray(dataNums)
        throw new DataCharNotInSquareException()
      }
      
      dataNums(i * 2) = rowColPair(0)
      dataNums(i * 2 + 1) = rowColPair(1)
    }
    
    val sq = square.square
    val compDataNums = computeFunc(dataNums, sq)
    eraseArray(dataNums)
    
    val result = new Array[Char](data.length)
    for (i <- 0 until data.length) {
      val row = compDataNums(i * 2)
      val col = compDataNums(i * 2 + 1)
      if (row >= sq.length || col >= sq(row).length) {
        eraseArray(compDataNums)
        eraseArray(result)
        throw new DataCharNotInSquareException()
      } else {
        var resCh = sq(row)(col)
        if (Character.isUpperCase(data.charAt(i))) {
          resCh = Character.toUpperCase(resCh)
        }
        result(i) = resCh
      }
    }
    
    eraseArray(compDataNums)
    result
  }
  
  private def computeCoords(ch: Char, square: Square, rowColPair: Array[Byte]): Option[Array[Byte]] = {
    val sq = square.square
    for (row <- 0 until sq.length) {
      for (col <- 0 until sq(row).length) {
        if (sq(row)(col) == ch) {
          rowColPair(0) = row.toByte
          rowColPair(1) = col.toByte
          return Some(rowColPair)
        }
      }
    }
    
    val mappingCh = square.missedOnExisting.get(ch)
    if (mappingCh.isEmpty) None else computeCoords(mappingCh.get, square, rowColPair)
  }
                                        
  def lowerSymbol(data: Array[Byte], square: Array[Array[Char]]) = {
    val result = new Array[Byte](data.length)
    for (i <- 0 until data.length by 2) {
      val row = data(i)
      val col = data(i + 1)
      
      var newRow = addByModulo(row, 1, square.length)
      if (col >= square(newRow).length) 
        newRow = 0
        
      result(i) = newRow.toByte
      result(i + 1) = col
    }
    result
  }
  
  def upperSymbol(data: Array[Byte], square: Array[Array[Char]]) = {
    val result = new Array[Byte](data.length)
    for (i <- 0 until data.length by 2) {
      val row = data(i)
      val col = data(i + 1)
      
      var newRow = subtractByModulo(row, 1, square.length)
      if (col >= square(newRow).length) 
        newRow -= 1
        
      result(i) = newRow.toByte
      result(i + 1) = col
    }
    result
  }
  
  def rowsCols(data: Array[Byte], square: Array[Array[Char]]) = {
    val result = new Array[Byte](data.length)
    val middle = data.length / 2
    for (i <- 0 until middle) {
      val row = data(i * 2)
      val col = data(i * 2 + 1)
      result(i) = row
      result(middle + i) = col
    }
    result
  }
  
  def rowsColsReverse(data: Array[Byte], square: Array[Array[Char]]) = {
    val result = new Array[Byte](data.length)
    val middle = data.length / 2
    for (i <- 0 until middle) {
      val row = data(i)
      val col = data(middle + i)
      result(i * 2) = row
      result(i * 2 + 1) = col
    }
    result
  }
  
  def colsRows(data: Array[Byte], square: Array[Array[Char]]) = {
    val result = new Array[Byte](data.length)
    val middle = data.length / 2
    for (i <- 0 until middle) {
      val row = data(i * 2)
      val col = data(i * 2 + 1)
      result(i) = col
      result(middle + i) = row
    }
    result
  }
  
  def colsRowsReverse(data: Array[Byte], square: Array[Array[Char]]) = {
    val result = new Array[Byte](data.length)
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
