package snakefish.crypto
package cipher.historical

import utils.MathOps._
import utils.CryptoUtils._
import data.KeyCharNotInAlphabetException

object Vigenere {

  def encode(data: CharSequence, key: CharSequence, alphabet: String, strictMode: Boolean = false) = {
    cryptoFunc(data, key, alphabet, strictMode)(addByModulo)
  }

  def decode(data: CharSequence, key: CharSequence, alphabet: String, strictMode: Boolean = false) = {
    cryptoFunc(data, key, alphabet, strictMode)(subtractByModulo)
  }

  private def cryptoFunc(
    data: CharSequence,
    key: CharSequence,
    alphabet: String,
    strictMode: Boolean
  )(
    resIndexCalc: (Int, Int, Int) => Int
  ) = {
    val keyLength = key.length
    val keyF = { i: Int =>
      val keyCharIndex = key.charAt(i % keyLength)
      if (keyCharIndex < 0) throw new KeyCharNotInAlphabetException()
      alphabet.indexOf(keyCharIndex)
    }
    sumKeySeqWithText(keyF)(data, alphabet, strictMode, resIndexCalc)
  }

}
