package snakefish.crypto.key

import java.util.Scanner
import snakefish.crypto.utils.FileUtils
import java.io.File

object NumberKey {
  def from(key: Long) = new NumberKey(key)
  def fromFile(filePath: String) = new NumberKey(FileUtils.readNumber(filePath))
  def fromFile(file: File) = new NumberKey(FileUtils.readNumber(file))
}

case class NumberKey private (private val key: Long) extends Key {
  
  def toNumber = key
  
  def toFile(filePath: String) {
    FileUtils.writeNumber(key, filePath)
  }
  
  def toFile(file: File) {
    FileUtils.writeNumber(key, file)
  }
}
