package snakefish.crypto
package cipher.historical

import Trifid._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap

object Trifid {
  
  def apply(cube: Array[Array[Array[Char]]], period: Int, strictMode: Boolean = false) = 
    new Trifid(cube, period, strictMode)
  
  case class DataCharNotInCubeException(val position: Int) 
      extends RuntimeException(s"Data char at position $position is missing in cube")
  
  case class CoordinatesOutOfBoundsException(val position: Int, val table: Int, val row: Int, val col: Int)
      extends RuntimeException(s"Coordinates (table = $table; row = $row; column = $col) of char at position $position are out of cube bounds")
  
}

class Trifid(val cube: Array[Array[Array[Char]]], val period: Int, val strictMode: Boolean = false) {
  
  @throws(classOf[DataCharNotInCubeException])
  @throws(classOf[CoordinatesOutOfBoundsException])
  def encode(data: CharSequence): String = compute(data, encodeBlock)

  @throws(classOf[DataCharNotInCubeException])
  @throws(classOf[CoordinatesOutOfBoundsException])
  def decode(data: CharSequence): String = compute(data, decodeBlock)
  
  private def compute(
    data: CharSequence,
    blockComputeFunc: (ArrayBuffer[Int], Array[Int]) => Unit
  ): String = {
    val dataNums = new ArrayBuffer[Int](data.length * 3)
    val notInSquareChars = new HashMap[Int, Char]()
    val coords = new Array[Int](3)
    
    for (i <- 0 until data.length) {
      val dataCh = data.charAt(i)
      if (compCoords(dataCh, coords)) {
        dataNums += coords(0)
        dataNums += coords(1)
        dataNums += coords(2)
      } else {
        if (strictMode) {
          throw new DataCharNotInCubeException(i)
        } else notInSquareChars.put(i, dataCh)
      }
    }
    
    applyBlockFunc(dataNums, blockComputeFunc)
    
    val result = new StringBuilder(data.length)
    var compInd = 0
    for (i <- 0 until data.length) {
      val notInSquareCh = notInSquareChars.get(i)
      if (notInSquareCh.isEmpty) {
        val table = dataNums(compInd * 3)
        val row = dataNums(compInd * 3 + 1)
        val col = dataNums(compInd * 3 + 2)
        
        if (table >= cube.length || row >= cube(table).length || col >= cube(table)(row).length)
          throw new CoordinatesOutOfBoundsException(i, table, row, col)
          
        var resCh = cube(table)(row)(col)
        if (data.charAt(i).isUpper) {
          resCh = resCh.toUpper
        }
        result += resCh
        compInd += 1
      } else result += notInSquareCh.get
    }

    result.toString
  }
  
  private def applyBlockFunc(
    data: ArrayBuffer[Int],
    blockComputeFunc: (ArrayBuffer[Int], Array[Int]) => Unit
  ): Unit = {
    val blockSize = 3 * period
    val fullBlocksCount = data.length / blockSize
    if (fullBlocksCount > 0) {
      val dataBlock = ArrayBuffer.fill(blockSize)(0)
      val compBlock = new Array[Int](blockSize)
      for (blockInd <- 0 until fullBlocksCount) {
        for (i <- 0 until blockSize) {
          dataBlock(i) = data(blockInd * blockSize + i)
        }
        blockComputeFunc(dataBlock, compBlock)
        for (i <- 0 until blockSize) {
          data(blockInd * blockSize + i) = compBlock(i)
        }  
      }
    }

    val lastChunkSize = data.length % blockSize
    if (lastChunkSize > 0) {
      val startIndex = data.length - lastChunkSize
      val dataChunk = data.slice(startIndex, data.length)
      val compChunk = new Array[Int](lastChunkSize)
      blockComputeFunc(dataChunk, compChunk)
      for (i <- 0 until lastChunkSize) {
        data(startIndex + i) = compChunk(i)
      }
    }
  }
  
  private def compCoords(ch: Char, coords: Array[Int]): Boolean = {
    val chLower = ch.toLower
    
    for (
      table <- 0 until cube.length;
      row   <- 0 until cube(table).length;
      col   <- 0 until cube(table)(row).length
    ) {
      if (chLower == cube(table)(row)(col).toLower) {
        coords(0) = table
        coords(1) = row
        coords(2) = col
        return true
      }
    }
    
    return false
  }
  
  private def encodeBlock(data: ArrayBuffer[Int], result: Array[Int]): Unit = {
    val oneThird = data.length / 3
    for (i <- 0 until oneThird) {
      val table = data(i * 3)
      val row = data(i * 3 + 1)
      val col = data(i * 3 + 2)
      result(i) = table
      result(oneThird + i) = row
      result(2 * oneThird + i) = col
    }
  }
  
  private def decodeBlock(data: ArrayBuffer[Int], result: Array[Int]): Unit = {
    val oneThird = data.length / 3
    for (i <- 0 until oneThird) {
      val table = data(i)
      val row = data(oneThird + i)
      val col = data(2 * oneThird + i)
      result(i * 3) = table
      result(i * 3 + 1) = row
      result(i * 3 + 2) = col
    }
  }
  
}
