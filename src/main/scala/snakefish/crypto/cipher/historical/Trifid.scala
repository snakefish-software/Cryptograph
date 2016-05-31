package snakefish.crypto
package cipher.historical

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object Trifid {
  
  class DataCharNotInCubeException() extends Exception("Data contains symbols that are missing in provided cube")
  
  def encode(data: CharSequence, cube: Array[Array[Array[Char]]], period: Int, strictMode: Boolean = false) = {
    compute(data, cube, period, encodeBlockFunc, strictMode)
  }

  def decode(data: CharSequence, cube: Array[Array[Array[Char]]], period: Int, strictMode: Boolean = false) = {
    compute(data, cube, period, decodeBlockFunc, strictMode)
  }
  
  private def compute(data: CharSequence,
                      cube: Array[Array[Array[Char]]],
                      period: Int,
                      blockComputeFunc: (ArrayBuffer[Int], Array[Int]) => Unit,
                      strictMode: Boolean) = {
    val dataNums = new ArrayBuffer[Int](data.length * 3)
    val notInSquareChars = new mutable.HashMap[Int, Char]()
    val coords = new Array[Int](3)
    
    for (i <- 0 until data.length) {
      val dataCh = data.charAt(i)
      if (compCoords(dataCh, cube, coords)) {
        dataNums += coords(0)
        dataNums += coords(1)
        dataNums += coords(2)
      } else {
        if (strictMode) {
          erase(coords)
          erase(dataNums)
          throw new DataCharNotInCubeException()
        } else notInSquareChars.put(i, dataCh)
      }
    }
    
    erase(coords)
    applyBlockFunc(dataNums, period, blockComputeFunc)
    
    val result = new Array[Char](data.length)
    var compInd = 0
    for (i <- 0 until data.length) {
      val notInSquareCh = notInSquareChars.get(i)
      if (notInSquareCh.isEmpty) {
        val table = dataNums(compInd * 3)
        val row = dataNums(compInd * 3 + 1)
        val col = dataNums(compInd * 3 + 2)
        if (table >= cube.length || row >= cube(table).length || col >= cube(table)(row).length) {
          erase(dataNums)
          erase(result)
          throw new DataCharNotInCubeException()
        } else {
          var resCh = cube(table)(row)(col)
          if (data.charAt(i).isUpper) {
            resCh = resCh.toUpper
          }
          result(i) = resCh
          compInd += 1
        }
      } else result(i) = notInSquareCh.get
    }

    erase(dataNums)
    result
  }
  
  private def applyBlockFunc(data: ArrayBuffer[Int], period: Int, blockComputeFunc: (ArrayBuffer[Int], Array[Int]) => Unit): Unit = {
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
      erase(dataBlock)
      erase(compBlock)
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
      erase(dataChunk)
      erase(compChunk)
    }
  }
  
  private def encodeBlockFunc(data: ArrayBuffer[Int], result: Array[Int]): Unit = {
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
  
  private def decodeBlockFunc(data: ArrayBuffer[Int], result: Array[Int]): Unit = {
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
  
  private def compCoords(ch: Char, cube: Array[Array[Array[Char]]], coords: Array[Int]): Boolean = {
    val chLower = ch.toLower
    for (table <- 0 until cube.length) {
      for (row <- 0 until cube(table).length) {
        for (col <- 0 until cube(table)(row).length) {
          if (chLower == cube(table)(row)(col).toLower) {
            coords(0) = table
            coords(1) = row
            coords(2) = col
            return true
          }
        }
      }
    }
    return false
  }
  
}
