package snakefish.crypto

import scala.language.implicitConversions

case class KeyCharNotInAlphabetException(char: Char, position: Int)
    extends RuntimeException(s"Char '$char' at position $position is missing in alphabet")

case class DataCharNotInAlphabetException(char: Char, position: Int)
    extends RuntimeException(s"Char '$char' at position $position is missing in alphabet")

object Alphabet {
  
  val ENGLISH = Alphabet("abcdefghijklmnopqrstuvwxyz")
  val RUSSIAN = Alphabet("абвгдеёжзийклмнопрстуфхцчшщъыьэюя")
  val NUMBERS = Alphabet("0123456789")
  val PUNCTUATION = Alphabet(",;.!?-:")
  val SPACES = Alphabet(" \t\n")
  
  implicit def alphabetToStr(alphabet: Alphabet): String = alphabet.toString
}

case class Alphabet (private val _alphabet: String) {
  
  private val alphabet = _alphabet.toLowerCase.distinct
  
  def apply(i: Int): Char = alphabet(i)
  
  override def toString: String = alphabet
  
  def length: Int = alphabet.length
  
  def +(that: String): Alphabet = Alphabet(alphabet + that)
  def +(that: Alphabet): Alphabet = Alphabet(alphabet + that.alphabet)
  
  def indexOf(ch: Char): Int = alphabet.indexOf(ch.toLower)
  def contains(ch: Char): Boolean = alphabet.contains(ch.toLower)
  
}
