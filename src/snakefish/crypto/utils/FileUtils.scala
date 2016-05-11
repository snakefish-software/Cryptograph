package snakefish.crypto.utils

import java.io.File
import java.io.Closeable
import java.io.PrintWriter
import scala.io.Source

object FileUtils {
  
  private val DEFAULT_CHARSET = "UTF-8"
  
  def readNumber(filePath: String): BigInt = readNumber(new File(filePath))
  
  def readNumber(file: File): BigInt = BigInt(readString(file).trim)
  
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
    var source: Source = null
    try {
      source = Source.fromFile(file, calcCharset(charset))
      source.mkString
    } finally {
      close(source)
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
      writer = new PrintWriter(file, calcCharset(charset))
      writer.print(data)
    } finally {
      close(writer)
    }
  }
  
  def calcCharset(charset: String) = if (charset != null) charset else DEFAULT_CHARSET
  
  def close(source: Source) {
    if (source != null) {
      try {
        source.close()
      } catch {
        case e: Exception => println("Exception on source closing: " + e);
      }
    }
  }
  
  def close(closeable: Closeable) {
    if (closeable != null) {
      try {
        closeable.close()
      } catch {
        case e: Exception => println("Exception on stream closing: " + e);
      }
    }
  }
}
