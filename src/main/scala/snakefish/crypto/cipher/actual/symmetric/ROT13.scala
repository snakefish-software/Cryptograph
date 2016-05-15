package snakefish.crypto
package cipher.actual.symmetric

import utils.MathOps._
import utils.CryptoUtils._

object ROT13 {

  def encode(data: CharSequence, alphabet: String, strictMode: Boolean = false) =
    cryptoFunc(data, alphabet, strictMode)(addByModulo)

  def decode(data: CharSequence, alphabet: String, strictMode: Boolean = false) =
    cryptoFunc(data, alphabet, strictMode)(subtractByModulo)

  private def cryptoFunc(
    data: CharSequence,
    alphabet: String,
    strictMode: Boolean
  )(
    resIndexCalc: (Int, Int, Int) => Int
  ) = {
    sumKeySeqWithText(_ => 13)(data, alphabet, strictMode, resIndexCalc)
  }

}
