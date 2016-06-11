package snakefish.crypto
package cipher.historical

object Gronsfeld {
  def apply(alphabet: Alphabet, strictMode: Boolean = false) = 
    new Gronsfeld(alphabet, strictMode)
}

class Gronsfeld(val alphabet: Alphabet, val strictMode: Boolean = false) {
  
  @throws(classOf[DataCharNotInAlphabetException])
  def encode(data: CharSequence, key: Long): Array[Char] = 
    encode(data, toDigits(key))
  
  @throws(classOf[DataCharNotInAlphabetException])
  def encode(data: CharSequence, key: Array[Int]): Array[Char] = 
    sumKeySeqWithText(data, key, alphabet, strictMode)(addByModulo)
  
  @throws(classOf[DataCharNotInAlphabetException])
  def decode(data: CharSequence, key: Long): Array[Char] = 
    decode(data, toDigits(key))
  
  @throws(classOf[DataCharNotInAlphabetException])
  def decode(data: CharSequence, key: Array[Int]): Array[Char] = 
    sumKeySeqWithText(data, key, alphabet, strictMode)(subtractByModulo)
  
}
