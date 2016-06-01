package snakefish.crypto

import scala.language.implicitConversions

class KeyCharNotInAlphabetException()
    extends Exception("Key contains symbols that are missing in provided alphabet")

class DataCharNotInAlphabetException()
    extends Exception("Data contains symbols that are missing in provided alphabet")

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
  
  def apply(i: Int) = alphabet(i)
  
  override def toString = alphabet
  
  def length = alphabet.length
  
  def +(that: String) = Alphabet(alphabet + that)
  
  def +(that: Alphabet) = Alphabet(alphabet + that.alphabet)
  
  def indexOf(ch: Char) = alphabet.indexOf(ch.toLower)
  
  def contains(ch: Char) = alphabet.contains(ch.toLower)
  
}
