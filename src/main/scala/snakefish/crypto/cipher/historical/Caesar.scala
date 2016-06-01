package snakefish.crypto
package cipher.historical

object Caesar {

  def encode(data: CharSequence, key: Int, alphabet: Alphabet, strictMode: Boolean = false) = {
    cryptoFunc(data, key, alphabet, strictMode)(addByModulo)
  }

  def decode(data: CharSequence, key: Int, alphabet: Alphabet, strictMode: Boolean = false) = {
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
