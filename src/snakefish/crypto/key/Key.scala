package snakefish.crypto.key

import java.io.File

/**
 * Base trait for all Key classes
 */
trait Key {
  
  def toFile(filePath: String)
  
  def toFile(file: File)
  
}
