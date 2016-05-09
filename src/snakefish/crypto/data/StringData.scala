package snakefish.crypto.data

import java.io.File
import snakefish.crypto.StringWrapper
import scala.io.Source
import snakefish.crypto.utils.FileUtils

object StringData {
  def apply(data: String) = new StringData(data)
  def fromFile(filePath: String) = new StringData(null, new File(filePath))
  def fromFile(filePath: String, charset: String) = new StringData(null, new File(filePath), charset)
  def fromFile(file: File) = new StringData(null, file)
  def fromFile(file: File, charset: String) = new StringData(null, file, charset)
}

class StringData private (private val data: String, private val file: File = null, private val charset: String = null)
  extends Iterator[Char] with Data with StringWrapper {
  
  private var currDataIndex = -1
  private var source: Source = null
  
  override def toString = data
  
  def hasNext = {
    var hasNext: Boolean = false
    if (data != null) {
      hasNext = currDataIndex + 1 < data.length
      if (!hasNext) {
        currDataIndex = -1
      }
    } else {
      if (source == null) {
        source = Source.fromFile(file, FileUtils.calcCharset(charset))
      }
      hasNext = source.hasNext
      if (!hasNext) {
        FileUtils.close(source)
        source = null
      }
    }
    hasNext
  }
  
  def next(): Char = {
    if (data != null) {
      currDataIndex += 1
      data(currDataIndex)
    } else {
      source.next()
    }
  }
  
}
