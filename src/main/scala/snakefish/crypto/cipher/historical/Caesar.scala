package snakefish.crypto
package cipher.historical

object Caesar {

  @throws(classOf[DataCharNotInAlphabetException])
  def encode(
    data: CharSequence,
    key: Int,
    alphabet: Alphabet,
    strictMode: Boolean = false
  ): Array[Char] = {
    cryptoFunc(data, key, alphabet, strictMode)(addByModulo)
  }

  @throws(classOf[DataCharNotInAlphabetException])
  def decode(
    data: CharSequence,
    key: Int,
    alphabet: Alphabet,
    strictMode: Boolean = false
  ): Array[Char] = {
    cryptoFunc(data, key, alphabet, strictMode)(subtractByModulo)
  }

  private def cryptoFunc(
    data: CharSequence,
    key: Int,
    alphabet: Alphabet,
    strictMode: Boolean
  )(
    resIndexCalc: (Int, Int, Int) => Int
  ) = {
    sumKeySeqWithText(_ => key)(data, alphabet, strictMode, resIndexCalc)
  }

}
