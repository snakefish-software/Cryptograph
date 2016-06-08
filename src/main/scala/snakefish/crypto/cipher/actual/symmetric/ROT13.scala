package snakefish.crypto
package cipher.actual.symmetric

object ROT13 {

  @throws(classOf[DataCharNotInAlphabetException])
  def encode(data: CharSequence, alphabet: Alphabet, strictMode: Boolean = false): Array[Char] =
    cryptoFunc(data, alphabet, strictMode)(addByModulo)

  @throws(classOf[DataCharNotInAlphabetException])
  def decode(data: CharSequence, alphabet: Alphabet, strictMode: Boolean = false): Array[Char] =
    cryptoFunc(data, alphabet, strictMode)(subtractByModulo)

  private def cryptoFunc(
    data: CharSequence,
    alphabet: Alphabet,
    strictMode: Boolean
  )(
    resIndexCalc: (Int, Int, Int) => Int
  ) = {
    sumKeySeqWithText(_ => 13)(data, alphabet, strictMode, resIndexCalc)
  }

}
