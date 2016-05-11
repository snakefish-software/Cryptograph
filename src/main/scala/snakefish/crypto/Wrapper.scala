package snakefish.crypto

import java.io.File

trait Wrapper {
  
  def toFile(filePath: String)
  
  def toFile(file: File)
  
}
