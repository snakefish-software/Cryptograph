package snakefish.crypto
package cipher.historical

import PolybiusSquare._
import scala.collection.mutable.ArrayBuffer

object Bifid {

  @throws(classOf[DataCharNotInSquareException])
  @throws(classOf[CoordinatesOutOfBoundsException])
  def encode(
    data: CharSequence,
    square: PolybiusSquare,
    period: Int,
    strictMode: Boolean = false
  ): Array[Char] = {
    PolybiusSquare.compute(data, square, computeFunc(period, rowsCols), strictMode)
  }

  @throws(classOf[DataCharNotInSquareException])
  @throws(classOf[CoordinatesOutOfBoundsException])
  def decode(
    data: CharSequence,
    square: PolybiusSquare,
    period: Int,
    strictMode: Boolean = false
  ): Array[Char] = {
    PolybiusSquare.compute(data, square, computeFunc(period, rowsColsReverse), strictMode)
  }

  private def computeFunc(period: Int, blockComputeFunc: (ArrayBuffer[Int], PolybiusSquare) => Array[Int]) = {
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
