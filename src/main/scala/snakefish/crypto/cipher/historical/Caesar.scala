package snakefish.crypto
package cipher.historical

object Caesar {
  def apply(alphabet: Alphabet, strictMode: Boolean = false) = 
    new Caesar(alphabet, strictMode)
}

class Caesar(val alphabet: Alphabet, val strictMode: Boolean = false) {

  @throws(classOf[DataCharNotInAlphabetException])
  def encode(data: CharSequence, key: Int): Array[Char] = 
    cryptoFunc(data, key)(addByModulo)

  @throws(classOf[DataCharNotInAlphabetException])
  def decode(data: CharSequence, key: Int): Array[Char] = 
    cryptoFunc(data, key)(subtractByModulo)

  private def cryptoFunc(
    data: CharSequence,
    key: Int
  )(
    resIndexCalc: (Int, Int, Int) => Int
  ): Array[Char] = {
    sumKeySeqWithText(_ => key)(data, alphabet, strictMode, resIndexCalc)
  }

}
