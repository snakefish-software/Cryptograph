package snakefish.alt

import scala.language.implicitConversions

case class KeyCharNotInAlphabetException(position: Int)
    extends RuntimeException(s"Key char at position $position is missing in alphabet")

case class DataCharNotInAlphabetException(position: Int)
    extends RuntimeException(s"Data char at position $position is missing in alphabet")

object Alphabet {
  
  val ENGLISH = Alphabet("abcdefghijklmnopqrstuvwxyz")
  val RUSSIAN = Alphabet("абвгдеёжзийклмнопрстуфхцчшщъыьэюя")
  val NUMBERS = Alphabet("0123456789")
  val PUNCTUATION = Alphabet(",;.!?-:")
  val SPACES = Alphabet(" \t\n")
  
  implicit def alphabetToStr(alphabet: Alphabet): String = alphabet.toString

  def apply(rawAlphabet: String): Alphabet =
    new Alphabet(rawAlphabet.toLowerCase.distinct)
}


class Alphabet(val alphabet: String) {

  def apply(i: Int): Char = alphabet(i)
  def length: Int = alphabet.length
  
  def +(that: String): Alphabet = Alphabet(alphabet + that)
  def +(that: Alphabet): Alphabet = Alphabet(alphabet + that.alphabet)
  
  def indexOf(ch: Char): Int = alphabet.indexOf(ch.toLower)
  def contains(ch: Char): Boolean = alphabet.contains(ch.toLower)

  override def toString: String = alphabet
}
