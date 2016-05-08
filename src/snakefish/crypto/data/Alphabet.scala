package snakefish.crypto.data

import snakefish.crypto.StringWrapper

object Alphabet {
  // TODO: DS: implement this
}

case class Alphabet(private val alphabet: String) extends StringWrapper {
  
  override def toString = alphabet
  
}
