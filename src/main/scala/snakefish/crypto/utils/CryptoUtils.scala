package snakefish.crypto.utils

import snakefish.crypto.data.DataCharNotInAlphabetException
import java.util.Random

object CryptoUtils {
  
  def sumKeySeqWithText(keyProvider: (Int) => Int) (data: CharSequence, alphabet: String, useStrictMode: Boolean, resIndexCalc: (Int, Int, Int) => Int) = {
    val dataLen = data.length()
    val result = new Array[Char](dataLen)
    
    val alphabetNorm = alphabet.toLowerCase
    
    for (i <- 0 until dataLen) {
      val dataCh = data.charAt(i)
      val isUpper = dataCh.isUpper
      val ch = if (isUpper) dataCh.toLower else dataCh
      
      val chIndex = alphabetNorm.indexOf(ch)
      
      if (chIndex >= 0) {
        val resIndex = resIndexCalc(chIndex, keyProvider(i), alphabetNorm.length)
        val resCh = if (isUpper) alphabetNorm(resIndex).toUpper else alphabetNorm(resIndex)
        result(i) = resCh
      } else {
        if (useStrictMode) throw new DataCharNotInAlphabetException
        else result(i) = dataCh
      }
    }
    
    new String(result, 0, dataLen)
  }
  
}
