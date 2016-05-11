package snakefish.crypto.key

import snakefish.crypto.utils.FileUtils
import java.io.File
import snakefish.crypto.StringWrapper

object StringKey {
  def apply(key: String) = new StringKey(key)
  def fromFile(filePath: String) = StringKey(FileUtils.readString(filePath))
  def fromFile(filePath: String, charset: String) = StringKey(FileUtils.readString(filePath, charset))
  def fromFile(file: File) = StringKey(FileUtils.readString(file))
  def fromFile(file: File, charset: String) = StringKey(FileUtils.readString(file, charset))
}

class StringKey private (private val key: String) extends Key with StringWrapper {
  
  override def toString = key
  
}
