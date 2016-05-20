package snakefish.crypto.utils

import snakefish.crypto.data.DataCharNotInAlphabetException

object CryptoUtils {

  def sumKeySeqWithText(keyProvider: Int => Int)(
    data: CharSequence,
    alphabet: String,
    useStrictMode: Boolean,
    resIndexCalc: (Int, Int, Int) => Int
  ) = {
    val dataLen = data.length()
    val alphabetNorm = alphabet.toLowerCase
    val alphabetNormLength = alphabetNorm.length
    val result = new Array[Char](dataLen)
    
    for (i <- 0 until dataLen) {
      val dataCh = data.charAt(i)
      val isUpper = Character.isUpperCase(dataCh)
      val ch = if (isUpper) Character.toLowerCase(dataCh) else dataCh
      
      val chIndex = alphabetNorm.indexOf(ch)
      
      if (chIndex >= 0) {
        val resIndex = resIndexCalc(chIndex, keyProvider(i), alphabetNormLength)
        val resCh = if (isUpper) Character.toUpperCase(alphabetNorm(resIndex)) else alphabetNorm(resIndex)
        result(i) = resCh
      } else {
        if (useStrictMode) {
          eraseArray(result)
          throw new DataCharNotInAlphabetException()
        } else result(i) = dataCh
      }
    }
    
    result
  }
  
  def eraseArray(arr: Array[Int]): Unit = {
    for (i <- 0 until arr.length) arr(i) = 0
  }
  
  def eraseArray(arr: Array[Byte]): Unit = {
    for (i <- 0 until arr.length) arr(i) = 0
  }
  
  def eraseArray(arr: Array[Char]): Unit = {
    val eraseCh = 0.toChar
    for (i <- 0 until arr.length) arr(i) = eraseCh
  }
  
}
