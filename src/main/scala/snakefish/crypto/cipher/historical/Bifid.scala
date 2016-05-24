package snakefish.crypto
package cipher.historical

import PolybiusSquare._
import utils.CryptoUtils._
import scala.collection.mutable.ArrayBuffer

object Bifid {

  def encode(data: CharSequence, square: PolybiusSquare, period: Int, strictMode: Boolean = false) = {
    PolybiusSquare.compute(data, square, computeFunc(period, rowsCols), strictMode)
  }

  def decode(data: CharSequence, square: PolybiusSquare, period: Int, strictMode: Boolean = false) = {
    PolybiusSquare.compute(data, square, computeFunc(period, rowsColsReverse), strictMode)
  }

  private def computeFunc(period: Int, blockComputeFunc: (ArrayBuffer[Int], Array[Array[Char]]) => Array[Int]) = {
    (data: ArrayBuffer[Int], square: Array[Array[Char]]) =>
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
            erase(compBlock)
          }
          erase(dataBlock)
        }

        val lastChunkSize = data.length % blockSize
        if (lastChunkSize > 0) {
          val startIndex = data.length - lastChunkSize
          val lastChunk = data.slice(startIndex, data.length)
          val compLastChunk = blockComputeFunc(lastChunk, square)
          for (i <- 0 until lastChunkSize) {
            result(startIndex + i) = compLastChunk(i)
          }
          erase(lastChunk)
          erase(compLastChunk)
        }

        result
      }
  }

}
