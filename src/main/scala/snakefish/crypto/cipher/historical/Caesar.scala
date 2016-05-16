package snakefish.crypto
package cipher.historical

import utils.MathOps._
import utils.CryptoUtils._

object Caesar {

  def encode(data: CharSequence, key: Int, alphabet: String, strictMode: Boolean = false) = {
    cryptoFunc(data, key, alphabet, strictMode)(addByModulo)
  }

  def decode(data: CharSequence, key: Int, alphabet: String, strictMode: Boolean = false) = {
    cryptoFunc(data, key, alphabet, strictMode)(subtractByModulo)
  }

  private def cryptoFunc(
    data: CharSequence,
    key: Int,
    alphabet: String,
    strictMode: Boolean
  )(
    resIndexCalc: (Int, Int, Int) => Int
  ) = {
    sumKeySeqWithText(_ => key)(data, alphabet, strictMode, resIndexCalc)
  }

}
