package snakefish.alt

import scala.language.implicitConversions

case class KeyCharNotInAlphabetException(position: Int)
    extends RuntimeException(s"Key char at position $position is missing in alphabet")

case class DataCharNotInAlphabetException(position: Int)
    extends RuntimeException(s"Data char at position $position is missing in alphabet")

sealed trait Alphabet {
  def chars: String
  def apply(i: Int): Char
  def length: Int
  def +(that: String): Alphabet
  def +(that: Alphabet): Alphabet
  def indexOf(ch: Char): Int
  def contains(ch: Char): Boolean
}

object Alphabet {

  private class AlphabetImpl(val chars: String) extends Alphabet {
    def apply(i: Int): Char = chars(i)
    def length: Int = chars.length

    def +(that: String): Alphabet = Alphabet(chars + that)
    def +(that: Alphabet): Alphabet = Alphabet(chars + that.chars)
    
    def indexOf(ch: Char): Int = chars.indexOf(ch.toLower)
    def contains(ch: Char): Boolean = chars.contains(ch.toLower)
    override def toString: String = chars
  }

  val ENGLISH = Alphabet("abcdefghijklmnopqrstuvwxyz")
  val RUSSIAN = Alphabet("абвгдеёжзийклмнопрстуфхцчшщъыьэюя")
  val NUMBERS = Alphabet("0123456789")
  val PUNCTUATION = Alphabet(",;.!?-:")
  val SPACES = Alphabet(" \t\n")
  
  implicit def alphabetToStr(alphabet: Alphabet): String = alphabet.toString

  def apply(rawAlphabet: String): Alphabet = {
    new AlphabetImpl(rawAlphabet.toLowerCase.distinct)
  }

}
