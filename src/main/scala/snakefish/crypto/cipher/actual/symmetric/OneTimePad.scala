package snakefish.crypto
package cipher.actual.symmetric

object OneTimePad {
  
  class KeyLengthInsuffisientException() extends Exception("Key length must be >= data length")
  
  def compute(data: Array[Byte], key: Array[Byte]): Array[Byte] = {
    if (key.length < data.length)
      throw new KeyLengthInsuffisientException()
    
    xor(data, key)
  }
  
  def encode(data: CharSequence, key: CharSequence, alphabet: Alphabet): Array[Char] = {
    cryptoFunc(data, key, alphabet)(addByModulo)
  }
  
  def decode(data: CharSequence, key: CharSequence, alphabet: Alphabet): Array[Char] = {
    cryptoFunc(data, key, alphabet)(subtractByModulo)
  }
  
  private def cryptoFunc(
    data: CharSequence,
    key: CharSequence,
    alphabet: Alphabet
  )(
    resIndexCalc: (Int, Int, Int) => Int
  ): Array[Char] = {
    if (key.length < data.length)
      throw new KeyLengthInsuffisientException()
    
    val result = new Array[Char](data.length)
    for (i <- 0 until data.length) {
      val dataChInd = alphabet.indexOf(data.charAt(i))
      if (dataChInd < 0) {
        erase(result)
        throw new DataCharNotInAlphabetException()
      }
      val keyChInd = alphabet.indexOf(key.charAt(i))
      if (keyChInd < 0) {
        erase(result)
        throw new KeyCharNotInAlphabetException()
      }
      val resChInd = resIndexCalc(dataChInd, keyChInd, alphabet.length)
      result(i) = alphabet(resChInd)
    }
    result
  }
  
}
