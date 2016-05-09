package snakefish.crypto.data

import snakefish.crypto.StringWrapper
import snakefish.crypto.utils.FileUtils
import java.io.File

object Alphabet {
  val ENGLISH = Alphabet("abcdefghijklmnopqrstuvwxyz")
  val RUSSIAN = Alphabet("абвгдеёжзийклмнопрстуфхцчшщъыьэюя")
  val NUMBERS = Alphabet("0123456789")
  val PUNCTUATION = Alphabet(",;.!?-:")
  val SPACES = Alphabet(" \t\n")
  
  def apply(alphabet: String) = new Alphabet(alphabet)
  def fromFile(filePath: String) = Alphabet(FileUtils.readString(filePath))
  def fromFile(filePath: String, charset: String) = Alphabet(FileUtils.readString(filePath, charset))
  def fromFile(file: File) = Alphabet(FileUtils.readString(file))
  def fromFile(file: File, charset: String) = Alphabet(FileUtils.readString(file, charset))
}

class Alphabet private (private val _alphabet: String) extends StringWrapper {
  
  private val alphabet = _alphabet.distinct
  
  override def toString = alphabet
  
  def length = alphabet.length
  
  def +(that: String) = Alphabet(alphabet + that)
  
  def +(that: Alphabet) = Alphabet(alphabet + that.alphabet)
  
}
