package snakefish.crypto
package cipher.classical

import Trifid._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap

object Trifid {
  
  def apply(cube: Array[Array[Array[Char]]], period: Int, strictMode: Boolean = false) = 
    new Trifid(cube, period, strictMode)
  
  case class DataCharNotInCubeException(position: Int) 
      extends RuntimeException(s"Data char at position $position is missing in cube")
  
  case class CoordinatesOutOfBoundsException(position: Int, table: Int, row: Int, col: Int)
      extends RuntimeException(s"Coordinates (table = $table; row = $row; column = $col) of char at position $position are out of cube bounds")
  
}

class Trifid(val cube: Array[Array[Array[Char]]], val period: Int, val strictMode: Boolean = false) {
  
  @throws(classOf[DataCharNotInCubeException])
  @throws(classOf[CoordinatesOutOfBoundsException])
  def encrypt(plaintext: CharSequence): String = 
    crypt(plaintext)(encryptBlock)

  @throws(classOf[DataCharNotInCubeException])
  @throws(classOf[CoordinatesOutOfBoundsException])
  def decrypt(ciphertext: CharSequence): String = 
    crypt(ciphertext)(decryptBlock)
  
  private def crypt(data: CharSequence)(blockCrypt: (ArrayBuffer[Int], Array[Int]) => Unit): String = {
    val dataNums = new ArrayBuffer[Int](data.length * 3)
    val notInSquareChars = new HashMap[Int, Char]()
    val coords = new Array[Int](3)
    
    for (i <- 0 until data.length) {
      val dataChar = data.charAt(i)
      if (compCoords(dataChar, coords)) {
        dataNums += coords(0)
        dataNums += coords(1)
        dataNums += coords(2)
      } else {
        if (strictMode) throw new DataCharNotInCubeException(i)
        else notInSquareChars.put(i, dataChar)
      }
    }
    
    applyBlockFunc(dataNums, blockCrypt)
    
    val result = new StringBuilder(data.length)
    var compIndex = 0
    for (i <- 0 until data.length) {
      val notInSquareChar = notInSquareChars.get(i)
      if (notInSquareChar.isEmpty) {
        val table = dataNums(compIndex * 3)
        val row = dataNums(compIndex * 3 + 1)
        val col = dataNums(compIndex * 3 + 2)
        
        if (table >= cube.length || row >= cube(table).length || col >= cube(table)(row).length)
          throw new CoordinatesOutOfBoundsException(i, table, row, col)
          
        var resultChar = cube(table)(row)(col)
        if (data.charAt(i).isUpper) {
          resultChar = resultChar.toUpper
        }
        result += resultChar
        compIndex += 1
      } else result += notInSquareChar.get
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
      for (blockIndex <- 0 until fullBlocksCount) {
        for (i <- 0 until blockSize) {
          dataBlock(i) = data(blockIndex * blockSize + i)
        }
        blockComputeFunc(dataBlock, compBlock)
        for (i <- 0 until blockSize) {
          data(blockIndex * blockSize + i) = compBlock(i)
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
    val charLower = ch.toLower
    for {
      table <- 0 until cube.length
      row   <- 0 until cube(table).length
      col   <- 0 until cube(table)(row).length
    } {
      if (charLower == cube(table)(row)(col).toLower) {
        coords(0) = table
        coords(1) = row
        coords(2) = col
        return true
      }
    }
    
    false
  }
  
  private def encryptBlock(data: ArrayBuffer[Int], result: Array[Int]): Unit = {
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
  
  private def decryptBlock(data: ArrayBuffer[Int], result: Array[Int]): Unit = {
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
