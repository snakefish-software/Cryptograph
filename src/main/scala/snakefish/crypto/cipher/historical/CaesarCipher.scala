package snakefish.crypto.cipher.historical

import snakefish.crypto.utils.MathOps._
import snakefish.crypto.data.DataCharNotInAlphabetException

object CaesarCipher {
  
  def encode(data: CharSequence, key: Int, alphabet: String, useStrictMode: Boolean = false) = {
    process(data, key, alphabet, useStrictMode, addByModulo)
  }
  
  def decode(data: CharSequence, key: Int, alphabet: String, useStrictMode: Boolean = false) = {
    process(data, key, alphabet, useStrictMode, subtractByModulo)
  }
  
  private def process(data: CharSequence, key: Int, alphabet: String, useStrictMode: Boolean, resIndexCalc: (Int, Int, Int) => Int) = {
    val result: StringBuilder = new StringBuilder
    
    val alphabetNorm = alphabet.toLowerCase
    
    for (i <- 0 until data.length()) {
      val dataCh = data.charAt(i)
      val isUpper = dataCh.isUpper
      val ch = if (isUpper) dataCh.toLower else dataCh
      
      val chIndex = alphabetNorm.indexOf(ch)
      
      if (chIndex >= 0) {
        val resIndex = resIndexCalc(chIndex, key, alphabetNorm.length)
        val resCh = if (isUpper) alphabetNorm(resIndex).toUpper else alphabetNorm(resIndex)
        result.append(resCh)
      } else {
        if (useStrictMode) throw new DataCharNotInAlphabetException
        else result.append(dataCh)
      }
    }
    
    result.toString
  }
  
}
