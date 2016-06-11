package snakefish.crypto
package cipher.actual.symmetric

object ROT13 {
  def apply(alphabet: Alphabet, strictMode: Boolean = false) = 
    new ROT13(alphabet, strictMode)
}

class ROT13(val alphabet: Alphabet, val strictMode: Boolean = false) {

  @throws(classOf[DataCharNotInAlphabetException])
  def encode(data: CharSequence): Array[Char] = 
    sumKeySeqWithText(_ => 13)(data, alphabet, strictMode, addByModulo)

  @throws(classOf[DataCharNotInAlphabetException])
  def decode(data: CharSequence): Array[Char] = 
    sumKeySeqWithText(_ => 13)(data, alphabet, strictMode, subtractByModulo)
  
}
