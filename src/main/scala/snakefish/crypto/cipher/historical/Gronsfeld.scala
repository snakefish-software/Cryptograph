package snakefish.crypto
package cipher.historical

import utils.CryptoUtils._

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
    Vigenere.encode(data, key, alphabet, strictMode)
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
    Vigenere.decode(data, key, alphabet, strictMode)
  }
  
}
