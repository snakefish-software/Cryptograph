package snakefish

import scala.collection.mutable.ArrayBuffer
import scala.language.higherKinds
import java.security.SecureRandom
import java.util.BitSet

package object crypto extends EraseInstances {

  def addByModulo(x: Int, y: Int, mod: Int) = (x % mod + y % mod) % mod

  def subtractByModulo(x: Int, y: Int, mod: Int) = (x % mod - y % mod + mod) % mod
  
  def xor(b1: Byte, b2: Byte): Byte = (b1 ^ b2).toByte
  
  def xor(ch1: Char, ch2: Char): Char = (ch1 ^ ch2).toChar
  
  def xor(data: Array[Byte], key: Array[Byte]): Array[Byte] = {
    val result = new Array[Byte](data.length)
    val keyLen = key.length
    for (i <- 0 until data.length) {
      result(i) = xor(data(i), key(i % keyLen))
    }
    result
  }
  
  def xor(data: CharSequence, key: CharSequence): Array[Char] = {
    val result = new Array[Char](data.length)
    val keyLen = key.length
    for (i <- 0 until data.length) {
      val dataCh = data.charAt(i)
      val keyCh = key.charAt(i % keyLen)
      result(i) = xor(dataCh, keyCh)
    }
    result
  }
  
  def sumKeySeqWithText(keyProvider: Int => Int)(
    data: CharSequence,
    alphabet: String,
    useStrictMode: Boolean,
    resIndexCalc: (Int, Int, Int) => Int
  ): Array[Char] = {
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
          throw new DataCharNotInAlphabetException()
        } else result(i) = dataCh
      }
    }
    
    result
  }
  
  def sumKeySeqWithText(
    data: CharSequence,
    key: Array[Int],
    alphabet: String,
    strictMode: Boolean
  )(
    resIndexCalc: (Int, Int, Int) => Int
  ): Array[Char] = {
    val keyLength = key.length
    val keyF = { i: Int => key(i % keyLength) }
    sumKeySeqWithText(keyF)(data, alphabet, strictMode, resIndexCalc)
  }
  
  def toDigits(num: Long) = {
    val digits = new ArrayBuffer[Int]()
    var numCopy = num
    while (numCopy > 0) {
      (numCopy % 10).toInt +=: digits
      numCopy = numCopy / 10
    }
    digits.toArray
  }

  def erase[R, E[R]](x: E[R])(implicit ev: Erase[E], evCh: EraseChar[R]): E[R] =
    ev.erase(x)(evCh)
  
  def shuffle(data: CharSequence, key: Long) = {
    val rand = new SecureRandom()
    rand.setSeed(key)
    val dataLen = data.length
    val result = new Array[Char](dataLen)
    
    for (i <- 0 until dataLen) {
      val pos = rand.nextInt(dataLen - i)
      var insPos = -1
      var emptyPosCounter = -1
      while (emptyPosCounter < pos) {
        insPos += 1
        if (result(insPos) == 0) {
          emptyPosCounter += 1
        }
      }
      result(insPos) = data.charAt(i)
    }
    
    result
  }
  
  def deshuffle(data: CharSequence, key: Long) = {
    val rand = new SecureRandom()
    rand.setSeed(key)
    val dataLen = data.length
    val result = new Array[Char](dataLen)
    val alreadyRead = new BitSet(dataLen)
    
    for (i <- 0 until dataLen) {
      val pos = rand.nextInt(dataLen - i)
      var readPos = -1
      var notReadCounter = -1
      while (notReadCounter < pos) {
        readPos += 1
        if (!alreadyRead.get(readPos)) {
          notReadCounter += 1
        }
      }
      result(i) = data.charAt(readPos)
      alreadyRead.set(readPos)
    }
    
    result
  }

}
