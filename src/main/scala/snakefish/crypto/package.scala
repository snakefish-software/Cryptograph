package snakefish

import snakefish.crypto.data.DataCharNotInAlphabetException
import scala.collection.mutable.ArrayBuffer

package object crypto {
  
  def addByModulo(x: Int, y: Int, mod: Int) = (x % mod + y % mod) % mod

  def subtractByModulo(x: Int, y: Int, mod: Int) = (x % mod - y % mod + mod) % mod
  
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
    
    var calcIndex = 0
    for (i <- 0 until dataLen) {
      val dataCh = data.charAt(i)
      val isUpper = dataCh.isUpper
      val ch = if (isUpper) dataCh.toLower else dataCh
      
      val chIndex = alphabetNorm.indexOf(ch)
      
      if (chIndex >= 0) {
        val resIndex = resIndexCalc(chIndex, keyProvider(calcIndex), alphabetNormLength)
        val resCh = if (isUpper) alphabetNorm(resIndex).toUpper else alphabetNorm(resIndex)
        result(i) = resCh
        calcIndex += 1
      } else {
        if (useStrictMode) {
          erase(result)
          throw new DataCharNotInAlphabetException()
        } else result(i) = dataCh
      }
    }
    
    result
  }
  
  def erase(arr: Array[Int]): Unit = {
    for (i <- 0 until arr.length) arr(i) = 0
  }
  
  def erase(arr: Array[Byte]): Unit = {
    for (i <- 0 until arr.length) arr(i) = 0
  }
  
  def erase(arr: Array[Char]): Unit = {
    val eraseCh = 0.toChar
    for (i <- 0 until arr.length) arr(i) = eraseCh
  }
  
  def erase(arr: ArrayBuffer[Int]): Unit = {
    for (i <- 0 until arr.length) arr(i) = 0
  }
  
  def eraze(arr: ArrayBuffer[Char]): Unit = {
    val eraseCh = 0.toChar
    for (i <- 0 until arr.length) arr(i) = eraseCh
  }
  
  def toDigits(num: Long) = {
    val digits = new ArrayBuffer[Int]()
    var numCopy = num
    while (numCopy > 0) {
      (numCopy % 10).toInt +=: digits
      numCopy = numCopy / 10
    }
    val result = digits.toArray
    erase(digits)
    result
  }
  
}
