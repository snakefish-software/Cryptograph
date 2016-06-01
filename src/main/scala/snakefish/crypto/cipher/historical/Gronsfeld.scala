package snakefish.crypto
package cipher.historical

object Gronsfeld {
  
  def encode(data: CharSequence, key: Long, alphabet: Alphabet): Array[Char] = {
    encode(data, key, alphabet, false)
  } 
  
  def encode(
    data: CharSequence,
    key: Long,
    alphabet: Alphabet,
    strictMode: Boolean): Array[Char] =
  {
    encode(data, toDigits(key), alphabet, strictMode)
  }
  
  def encode(data: CharSequence, key: Array[Int], alphabet: Alphabet): Array[Char] = {
    encode(data, key, alphabet, false)
  }
  
  def encode(
    data: CharSequence,
    key: Array[Int],
    alphabet: Alphabet,
    strictMode: Boolean): Array[Char] =
  {
    sumKeySeqWithText(data, key, alphabet, strictMode)(addByModulo)
  }
  
  def decode(data: CharSequence, key: Long, alphabet: Alphabet): Array[Char] = {
    decode(data, key, alphabet, false)
  }
  
  def decode(
    data: CharSequence,
    key: Long,
    alphabet: Alphabet,
    strictMode: Boolean): Array[Char] =
  {
    decode(data, toDigits(key), alphabet, strictMode)
  }
  
  def decode(data: CharSequence, key: Array[Int], alphabet: Alphabet): Array[Char] = {
    decode(data, key, alphabet, false)
  }
  
  def decode(
    data: CharSequence,
    key: Array[Int],
    alphabet: Alphabet,
    strictMode: Boolean): Array[Char] =
  {
    sumKeySeqWithText(data, key, alphabet, strictMode)(subtractByModulo)
  }
  
}
