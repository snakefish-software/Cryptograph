package snakefish.crypto
package cipher.historical

object Gronsfeld {
  
  def encode(data: CharSequence, key: Long, alphabet: String): Array[Char] = {
    encode(data, key, alphabet, false)
  } 
  
  def encode(data: CharSequence, key: Long, alphabet: String, strictMode: Boolean): Array[Char] = {
    val keyDigits = toDigits(key)
    val result = encode(data, keyDigits, alphabet, strictMode)
    erase(keyDigits)
    result
  }
  
  def encode(data: CharSequence, key: Array[Int], alphabet: String): Array[Char] = {
    encode(data, key, alphabet, false)
  }
  
  def encode(data: CharSequence, key: Array[Int], alphabet: String, strictMode: Boolean) = {
    sumKeySeqWithText(data, key, alphabet, strictMode)(addByModulo)
  }
  
  def decode(data: CharSequence, key: Long, alphabet: String): Array[Char] = {
    decode(data, key, alphabet, false)
  }
  
  def decode(data: CharSequence, key: Long, alphabet: String, strictMode: Boolean): Array[Char] = {
    val keyDigits = toDigits(key)
    val result = decode(data, keyDigits, alphabet, strictMode)
    erase(keyDigits)
    result
  }
  
  def decode(data: CharSequence, key: Array[Int], alphabet: String): Array[Char] = {
    decode(data, key, alphabet, false)
  }
  
  def decode(data: CharSequence, key: Array[Int], alphabet: String, strictMode: Boolean) = {
    sumKeySeqWithText(data, key, alphabet, strictMode)(subtractByModulo)
  }
  
}
