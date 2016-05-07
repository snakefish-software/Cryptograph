package snakefish.crypto

import java.io.File

trait ToFile {
  
  def toFile(filePath: String)
  
  def toFile(file: File)
  
}
