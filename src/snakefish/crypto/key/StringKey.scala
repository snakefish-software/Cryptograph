package snakefish.crypto.key

import snakefish.crypto.utils.FileUtils
import java.io.File
import snakefish.crypto.StringWrapper

object StringKey {
  def from(key: String) = new StringKey(key)
  def fromFile(filePath: String) = new StringKey(FileUtils.readString(filePath))
  def fromFile(filePath: String, charset: String) = new StringKey(FileUtils.readString(filePath, charset))
  def fromFile(file: File) = new StringKey(FileUtils.readString(file))
  def fromFile(file: File, charset: String) = new StringKey(FileUtils.readString(file, charset))
}

case class StringKey private (private val key: String) extends Key with StringWrapper {
  
  override def toString = key
  
}
