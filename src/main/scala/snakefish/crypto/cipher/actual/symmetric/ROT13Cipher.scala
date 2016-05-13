package snakefish.crypto.cipher.actual.symmetric

import snakefish.crypto.utils.MathOps._
import snakefish.crypto.utils.CryptoUtils

object ROT13Cipher {
  
  def encode(data: CharSequence, alphabet: String, useStrictMode: Boolean = false) = {
    process(data, alphabet, useStrictMode, addByModulo)
  }
  
  def decode(data: CharSequence, alphabet: String, useStrictMode: Boolean = false) = {
    process(data, alphabet, useStrictMode, subtractByModulo)
  }
  
  private def process(data: CharSequence, alphabet: String, useStrictMode: Boolean, resIndexCalc: (Int, Int, Int) => Int) = {
    val cryptoFunc = CryptoUtils.sumKeySeqWithText(_ => 13) _
    cryptoFunc(data, alphabet, useStrictMode, resIndexCalc)
  }
  
}
