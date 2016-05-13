package snakefish.crypto.cipher.historical

import snakefish.crypto.utils.MathOps._
import snakefish.crypto.utils.CryptoUtils

object CaesarCipher {
  
  def encode(data: CharSequence, key: Int, alphabet: String, useStrictMode: Boolean = false) = {
    process(data, key, alphabet, useStrictMode, addByModulo)
  }
  
  def decode(data: CharSequence, key: Int, alphabet: String, useStrictMode: Boolean = false) = {
    process(data, key, alphabet, useStrictMode, subtractByModulo)
  }
  
  private def process(data: CharSequence, key: Int, alphabet: String, useStrictMode: Boolean, resIndexCalc: (Int, Int, Int) => Int) = {
    val cryptoFunc = CryptoUtils.sumKeySeqWithText(_ => key) _
    cryptoFunc(data, alphabet, useStrictMode, resIndexCalc)
  }
  
}
