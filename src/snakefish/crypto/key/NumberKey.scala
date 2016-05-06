package snakefish.crypto.key

import java.util.Scanner
import snakefish.crypto.utils.FileUtils
import java.io.File

object NumberKey {
  def from(key: Long) = new NumberKey(key)
  def fromFile(filePath: String) = new NumberKey(filePath)
  def fromFile(file: File) = new NumberKey(file)
}

class NumberKey private (_key: Long) extends Key {
  private var key: Long = _key
  private var filePath: String = null
  
  private def this(_filePath: String) {
    this(FileUtils.readNumber(_filePath))
  }
  
  private def this(_file: File) {
    this(FileUtils.readNumber(_file))
  }
  
  def toNumber = key
  
  def toFile(filePath: String) {
    FileUtils.writeNumber(key, filePath)
  }
}
