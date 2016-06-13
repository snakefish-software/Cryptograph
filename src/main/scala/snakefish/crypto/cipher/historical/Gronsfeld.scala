package snakefish.crypto
package cipher.historical

object Gronsfeld {
  
  def apply(alphabet: Alphabet, strictMode: Boolean = false) = 
    new Gronsfeld(alphabet, strictMode)
}

class Gronsfeld(val alphabet: Alphabet, val strictMode: Boolean = false) {
  
  @throws(classOf[DataCharNotInAlphabetException])
  def encode(data: CharSequence, key: Long): String = 
    encode(data, toDigits(key))
  
  @throws(classOf[DataCharNotInAlphabetException])
  def encode(data: CharSequence, key: Array[Int]): String = 
    sumKeySeqWithText(data, key, alphabet, strictMode)(addByModulo)
  
  @throws(classOf[DataCharNotInAlphabetException])
  def decode(data: CharSequence, key: Long): String = 
    decode(data, toDigits(key))
  
  @throws(classOf[DataCharNotInAlphabetException])
  def decode(data: CharSequence, key: Array[Int]): String = 
    sumKeySeqWithText(data, key, alphabet, strictMode)(subtractByModulo)
  
}
