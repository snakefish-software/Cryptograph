package snakefish.crypto
package cipher.historical

import data.KeyCharNotInAlphabetException

object Vigenere {
  
  def encode(data: CharSequence, key: CharSequence, alphabet: String): Array[Char] = {
    encode(data, key, alphabet, false)
  }

  def encode(data: CharSequence, key: CharSequence, alphabet: String, strictMode: Boolean) = {
    cryptoFunc(data, key, alphabet, strictMode)(addByModulo)
  }
  
  def encode(data: CharSequence, key: Array[Int], alphabet: String): Array[Char] = {
    encode(data, key, alphabet, false)
  }
  
  def encode(data: CharSequence, key: Array[Int], alphabet: String, strictMode: Boolean) = {
    sumKeySeqWithText(data, key, alphabet, strictMode)(addByModulo)
  }
  
  def decode(data: CharSequence, key: CharSequence, alphabet: String): Array[Char] = {
    decode(data, key, alphabet, false)
  }

  def decode(data: CharSequence, key: CharSequence, alphabet: String, strictMode: Boolean) = {
    cryptoFunc(data, key, alphabet, strictMode)(subtractByModulo)
  }
  
  def decode(data: CharSequence, key: Array[Int], alphabet: String): Array[Char] = {
    decode(data, key, alphabet, false)
  }
  
  def decode(data: CharSequence, key: Array[Int], alphabet: String, strictMode: Boolean) = {
    sumKeySeqWithText(data, key, alphabet, strictMode)(subtractByModulo)
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
      val keyCh = key.charAt(i).toLower
      val keyChIndex = alphabetNorm.indexOf(keyCh)
      if (keyChIndex < 0) {
        erase(keyInts)
        throw new KeyCharNotInAlphabetException()
      } else keyInts(i) = keyChIndex
    }
    
    val result = sumKeySeqWithText(data, keyInts, alphabet, strictMode)(resIndexCalc)
    erase(keyInts)
    result
  }

}
