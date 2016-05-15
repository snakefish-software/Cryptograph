package snakefish.crypto.utils

import snakefish.crypto.data.DataCharNotInAlphabetException
import java.util.Random

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
      val isUpper = dataCh.isUpper
      val ch = if (isUpper) dataCh.toLower else dataCh
      
      val chIndex = alphabetNorm.indexOf(ch)
      
      if (chIndex >= 0) {
        val resIndex = resIndexCalc(chIndex, keyProvider(i), alphabetNormLength)
        val resCh = if (isUpper) alphabetNorm(resIndex).toUpper else alphabetNorm(resIndex)
        result(i) = resCh
      } else {
        if (useStrictMode) {
          eraseArray(result, i + 1)
          throw new DataCharNotInAlphabetException()
        } else result(i) = dataCh
      }
    }
    
    result
  }
  
  def eraseArray(arr: Array[Char], untilInd: Int): Unit = {
    val eraseCh = 0.toChar
    for (i <- 0 until untilInd) arr(i) = eraseCh
  }
  
}
