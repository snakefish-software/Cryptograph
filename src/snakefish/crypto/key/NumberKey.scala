package snakefish.crypto.key

import snakefish.crypto.utils.FileUtils
import java.io.File
import snakefish.crypto.NumberWrapper

object NumberKey {
  def from(key: Long) = new NumberKey(key)
  def fromFile(filePath: String) = new NumberKey(FileUtils.readNumber(filePath))
  def fromFile(file: File) = new NumberKey(FileUtils.readNumber(file))
}

case class NumberKey private (private val key: Long) extends Key with NumberWrapper {
  
  def toNumber = key
  
}
