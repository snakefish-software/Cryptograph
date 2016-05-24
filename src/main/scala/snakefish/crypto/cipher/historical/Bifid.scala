package snakefish.crypto
package cipher.historical

import PolybiusSquare._
import utils.CryptoUtils._
import scala.collection.mutable.ArrayBuffer

object Bifid {

  def encode(data: CharSequence, square: Square, period: Int, strictMode: Boolean = false) = {
    compute(data, square, computeFunc(period, rowsCols), strictMode)
  }

  def decode(data: CharSequence, square: Square, period: Int, strictMode: Boolean = false) = {
    compute(data, square, computeFunc(period, rowsColsReverse), strictMode)
  }

  private def computeFunc(period: Int, periodComputeFunc: (ArrayBuffer[Int], Array[Array[Char]]) => Array[Int]) = {
    (data: ArrayBuffer[Int], square: Array[Array[Char]]) =>
      {
        val result = new Array[Int](data.length)

        val periodSize = 2 * period
        val fullPeriodsCount = data.length / periodSize
        if (fullPeriodsCount > 0) {
          val dataPeriod = ArrayBuffer.fill(periodSize)(0)
          for (periodInd <- 0 until fullPeriodsCount) {
            for (i <- 0 until periodSize) {
              dataPeriod(i) = data(periodInd * periodSize + i)
            }
            val compPeriod = periodComputeFunc(dataPeriod, square)
            for (i <- 0 until periodSize) {
              result(periodInd * periodSize + i) = compPeriod(i)
            }
            erase(compPeriod)
          }
          erase(dataPeriod)
        }

        val lastChunkSize = data.length % periodSize
        if (lastChunkSize > 0) {
          val startIndex = data.length - lastChunkSize
          val lastChunk = data.slice(startIndex, data.length)
          val compLastChunk = periodComputeFunc(lastChunk, square)
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
