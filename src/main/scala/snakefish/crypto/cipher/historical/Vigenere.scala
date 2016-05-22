package snakefish.crypto
package cipher.historical

import utils.MathOps._
import utils.CryptoUtils._
import data.KeyCharNotInAlphabetException

object Vigenere {
  
  def encode(data: CharSequence, key: CharSequence, alphabet: String) = {
    cryptoFunc(data, key, alphabet, false)(addByModulo)
  }

  def encode(data: CharSequence, key: CharSequence, alphabet: String, strictMode: Boolean) = {
    cryptoFunc(data, key, alphabet, strictMode)(addByModulo)
  }
  
  def encode(data: CharSequence, key: Array[Int], alphabet: String) = {
    cryptoFunc(data, key, alphabet, false)(addByModulo)
  }
  
  def encode(data: CharSequence, key: Array[Int], alphabet: String, strictMode: Boolean) = {
    cryptoFunc(data, key, alphabet, strictMode)(addByModulo)
  }
  
  def decode(data: CharSequence, key: CharSequence, alphabet: String) = {
    cryptoFunc(data, key, alphabet, false)(subtractByModulo)
  }

  def decode(data: CharSequence, key: CharSequence, alphabet: String, strictMode: Boolean) = {
    cryptoFunc(data, key, alphabet, strictMode)(subtractByModulo)
  }
  
  def decode(data: CharSequence, key: Array[Int], alphabet: String) = {
    cryptoFunc(data, key, alphabet, false)(subtractByModulo)
  }
  
  def decode(data: CharSequence, key: Array[Int], alphabet: String, strictMode: Boolean) = {
    cryptoFunc(data, key, alphabet, strictMode)(subtractByModulo)
  }

  private def cryptoFunc(
    data: CharSequence,
    key: CharSequence,
    alphabet: String,
    strictMode: Boolean
  )(
    resIndexCalc: (Int, Int, Int) => Int
  ): Array[Char] = {
    val keyInts = new Array[Int](key.length)
    val alphabetNorm = alphabet.toLowerCase
    
    for (i <- 0 until key.length) {
      val keyCh = Character.toLowerCase(key.charAt(i))
      val keyChIndex = alphabetNorm.indexOf(keyCh)
      if (keyChIndex < 0) {
        eraseArray(keyInts)
        throw new KeyCharNotInAlphabetException()
      } else keyInts(i) = keyChIndex
    }
    
    val result = cryptoFunc(data, keyInts, alphabet, strictMode)(resIndexCalc)
    eraseArray(keyInts)
    result
  }
  
  private def cryptoFunc(
    data: CharSequence,
    key: Array[Int],
    alphabet: String,
    strictMode: Boolean
  )(
    resIndexCalc: (Int, Int, Int) => Int
  ): Array[Char] = {
    val keyLength = key.length
    val keyF = { i: Int => key(i % keyLength) }
    sumKeySeqWithText(keyF)(data, alphabet, strictMode, resIndexCalc)
  }

}
