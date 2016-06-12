package snakefish.crypto
package cipher.historical

object Caesar {
  def apply(alphabet: Alphabet, strictMode: Boolean = false) = 
    new Caesar(alphabet, strictMode)
}

class Caesar(val alphabet: Alphabet, val strictMode: Boolean = false) {

  @throws(classOf[DataCharNotInAlphabetException])
  def encode(data: CharSequence, key: Int): String = 
    sumKeySeqWithText(_ => key)(data, alphabet, strictMode, addByModulo)

  @throws(classOf[DataCharNotInAlphabetException])
  def decode(data: CharSequence, key: Int): String = 
    sumKeySeqWithText(_ => key)(data, alphabet, strictMode, subtractByModulo)

}
