package snakefish.crypto

import snakefish.crypto.utils.FileUtils
import java.io.File

trait StringWrapper extends Wrapper {
  
  def toString: String
  
  def toFile(filePath: String) {
    FileUtils.writeString(toString, filePath)
  }
  
  def toFile(filePath: String, charset: String) {
    FileUtils.writeString(toString, filePath, charset)
  }
  
  def toFile(file: File) {
    FileUtils.writeString(toString, file)
  }
  
  def toFile(file: File, charset: String) {
    FileUtils.writeString(toString, file, charset)
  }
  
}