package snakefish.crypto.key

import snakefish.crypto.utils.FileUtils
import java.io.File
import snakefish.crypto.NumberWrapper

object NumberKey {
  def apply(key: BigInt) = new NumberKey(key)
  def fromFile(filePath: String) = NumberKey(FileUtils.readNumber(filePath))
  def fromFile(file: File) = NumberKey(FileUtils.readNumber(file))
}

class NumberKey private (private val key: BigInt) extends Key with NumberWrapper {
  
  def toNumber = key
  
}
