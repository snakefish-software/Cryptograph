package snakefish.crypto

import snakefish.crypto.utils.FileUtils
import java.io.File

trait NumberWrapper extends Wrapper {
  
  def toNumber: Number
  
  def toFile(filePath: String) {
    FileUtils.writeNumber(toNumber, filePath)
  }
  
  def toFile(file: File) {
    FileUtils.writeNumber(toNumber, file)
  }
  
}