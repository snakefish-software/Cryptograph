package snakefish.crypto
package cipher.classical

import PolybiusSquare._
import scala.collection.mutable.ArrayBuffer

object Bifid {
  
  def apply(square: PolybiusSquare, period: Int, strictMode: Boolean = false) = 
    new Bifid(square, period, strictMode)
}

class Bifid(val square: PolybiusSquare, val period: Int, val strictMode: Boolean = false) {

  @throws(classOf[DataCharNotInSquareException])
  @throws(classOf[CoordinatesOutOfBoundsException])
  def encrypt(plaintext: CharSequence): String = 
    PolybiusSquare.compute(plaintext, square, computeFunc(rowsCols), strictMode)

  @throws(classOf[DataCharNotInSquareException])
  @throws(classOf[CoordinatesOutOfBoundsException])
  def decrypt(ciphertext: CharSequence): String = 
    PolybiusSquare.compute(ciphertext, square, computeFunc(rowsColsReverse), strictMode)

  private def computeFunc(blockComputeFunc: (ArrayBuffer[Int], PolybiusSquare) => Array[Int]) = {
    (data: ArrayBuffer[Int], square: PolybiusSquare) =>
      {
        val result = new Array[Int](data.length)

        val blockSize = 2 * period
        val fullBlocksCount = data.length / blockSize
        if (fullBlocksCount > 0) {
          val dataBlock = ArrayBuffer.fill(blockSize)(0)
          for (blockInd <- 0 until fullBlocksCount) {
            for (i <- 0 until blockSize) {
              dataBlock(i) = data(blockInd * blockSize + i)
            }
            val compBlock = blockComputeFunc(dataBlock, square)
            for (i <- 0 until blockSize) {
              result(blockInd * blockSize + i) = compBlock(i)
            }
          }
        }

        val lastChunkSize = data.length % blockSize
        if (lastChunkSize > 0) {
          val startIndex = data.length - lastChunkSize
          val dataChunk = data.slice(startIndex, data.length)
          val compChunk = blockComputeFunc(dataChunk, square)
          for (i <- 0 until lastChunkSize) {
            result(startIndex + i) = compChunk(i)
          }
        }

        result
      }
  }

}
