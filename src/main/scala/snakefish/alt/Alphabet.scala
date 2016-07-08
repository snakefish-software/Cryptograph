package snakefish.alt

import scala.language.implicitConversions

case class KeyCharNotInAlphabetException(position: Int)
    extends RuntimeException(s"Key char at position $position is missing in alphabet")

case class DataCharNotInAlphabetException(position: Int)
    extends RuntimeException(s"Data char at position $position is missing in alphabet")

class Alphabet(val chars: String) {

  def apply(i: Int): Char = chars(i)
  def length: Int = chars.length
  
  def +(that: String): Alphabet = Alphabet(chars + that)
  def +(that: Alphabet): Alphabet = Alphabet(chars + that.chars)
  
  def indexOf(ch: Char): Int = chars.indexOf(ch.toLower)
  def contains(ch: Char): Boolean = chars.contains(ch.toLower)
}

object Alphabet {
  
  val ENGLISH = Alphabet("abcdefghijklmnopqrstuvwxyz")
  val RUSSIAN = Alphabet("абвгдеёжзийклмнопрстуфхцчшщъыьэюя")
  val NUMBERS = Alphabet("0123456789")
  val PUNCTUATION = Alphabet(",;.!?-:")
  val SPACES = Alphabet(" \t\n")
  
  implicit def alphabetToStr(alphabet: Alphabet): String = alphabet.chars

  def apply(rawAlphabet: String): Alphabet =
    new Alphabet(rawAlphabet.toLowerCase.distinct)
}
