package snakefish.crypto.key

import snakefish.crypto.utils.FileUtils
import java.io.File

object StringKey {
  def from(key: String) = new StringKey(key)
  def fromFile(filePath: String) = new StringKey(FileUtils.readString(filePath))
  def fromFile(filePath: String, charset: String) = new StringKey(FileUtils.readString(filePath, charset))
  def fromFile(file: File) = new StringKey(FileUtils.readString(file))
  def fromFile(file: File, charset: String) = new StringKey(FileUtils.readString(file, charset))
}

case class StringKey private (private val key: String) extends Key {
  
  override def toString = key
  
  def toFile(filePath: String) {
    FileUtils.writeString(key, filePath)
  }
  
  def toFile(filePath: String, charset: String) {
    FileUtils.writeString(key, filePath, charset)
  }
  
  def toFile(file: File) {
    FileUtils.writeString(key, file)
  }
  
  def toFile(file: File, charset: String) {
    FileUtils.writeString(key, file, charset)
  }
  
}
