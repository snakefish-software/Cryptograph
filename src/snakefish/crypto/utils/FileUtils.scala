package snakefish.crypto.utils

import java.io.File
import java.util.Scanner
import java.io.Closeable
import java.io.PrintWriter

object FileUtils {
  
  val DEFAULT_CHARSET = "UTF-8"
  
  def readNumber(filePath: String): Long = readNumber(new File(filePath))
  
  def readNumber(file: File): Long = readString(file).trim.toLong
  
  def writeNumber(data: Number, filePath: String) {
    writeNumber(data, new File(filePath))
  }
  
  def writeNumber(data: Number, file: File) {
    writeString(data.toString, file)
  }
  
  def readString(filePath: String): String = readString(new File(filePath))
  
  def readString(file: File): String = readString(file, DEFAULT_CHARSET)
  
  def readString(filePath: String, charset: String): String = readString(new File(filePath), charset)
  
  def readString(file: File, charset: String): String = {
    var scanner: Scanner = null
    try {
      scanner = new Scanner(file, charset)
      scanner.useDelimiter("\\A").next
    } finally {
      close(scanner)
    }
  }
  
  def writeString(data: String, filePath: String) {
    writeString(data, new File(filePath))
  }
  
  def writeString(data: String, file: File) {
    writeString(data, file, DEFAULT_CHARSET)
  }
  
  def writeString(data: String, filePath: String, charset: String) {
    writeString(data, new File(filePath), charset)
  }
  
  def writeString(data: String, file: File, charset: String) {
    var writer: PrintWriter = null
    try {
      writer = new PrintWriter(file, charset)
      writer.print(data)
    } finally {
      close(writer)
    }
  }
  
  private def close(closeable: Closeable) {
    if (closeable != null) {
      try {
        closeable.close()
      } catch {
        case e: Exception => println("Exception on stream closing: " + e);
      }
    }
  }
}
