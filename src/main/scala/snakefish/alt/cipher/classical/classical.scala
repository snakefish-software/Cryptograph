package snakefish.alt
package cipher

import scala.collection.mutable.ArrayBuffer
import java.security.SecureRandom
import java.util.BitSet

package object classical {
  
  case class OddCiphertextLengthException()
      extends RuntimeException("Ciphertext length must be even")
  
  case class PlaceholderNotInSquareException()
      extends RuntimeException("Placeholder char is missing in Polybius square")
  
  def sumKeySeqWithText(keyProvider: Int => Int)(
    data: CharSequence,
    alphabet: Alphabet,
    strictMode: Boolean,
    resIndexCalc: (Int, Int, Int) => Int
  ): String = {
    val result = new StringBuilder(data.length)
    var calcIndex = 0
    
    for (i <- 0 until data.length) {
      val dataCh = data.charAt(i)
      val chIndex = alphabet.indexOf(dataCh)
      if (chIndex >= 0) {
        val resIndex = resIndexCalc(chIndex, keyProvider(calcIndex), alphabet.length)
        result += (if (dataCh.isUpper) alphabet(resIndex).toUpper else alphabet(resIndex))
        calcIndex += 1
      } else {
        if (strictMode) {
          throw new DataCharNotInAlphabetException(i)
        } else result += dataCh
      }
    }
    
    result.toString
  }
  
  def sumKeySeqWithText(
    key: Array[Int],
    data: CharSequence,
    alphabet: Alphabet,
    strictMode: Boolean
  )(
    resIndexCalc: (Int, Int, Int) => Int
  ): String = {
    val keyLength = key.length
    val keyF = { i: Int => if (keyLength > 0) key(i % keyLength) else 0 }
    sumKeySeqWithText(keyF)(data, alphabet, strictMode, resIndexCalc)
  }
  
  def indicesInAlphabet(chars: CharSequence, alphabet: Alphabet): Array[Int] = {
    val indices = new ArrayBuffer[Int](chars.length)
    for (i <- 0 until chars.length) {
      val chIndex = alphabet.indexOf(chars.charAt(i))
      if (chIndex >= 0)
        indices += chIndex
    }
    indices.toArray
  }
  
  def toDigits(num: Long): Array[Int] = {
    val digits = new ArrayBuffer[Int]()
    var numCopy = num
    while (numCopy > 0) {
      (numCopy % 10).toInt +=: digits
      numCopy = numCopy / 10
    }
    digits.toArray
  }
  
  def shuffle(key: Long, data: CharSequence): Array[Char] = {
    val rand = SecureRandom.getInstance("SHA1PRNG")
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
  
  def deshuffle(key: Long, data: CharSequence): Array[Char] = {
    val rand = SecureRandom.getInstance("SHA1PRNG")
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

  def splitAt(str: String, length: Int): Array[String] = {
    def splitRec(input: String, result: ArrayBuffer[String]): ArrayBuffer[String] = {
      input match {
        case "" => result
        case str =>
          val (head, rest) = input.splitAt(length)
          splitRec(rest, result :+ head)
      }
    }

    splitRec(str, new ArrayBuffer[String]).toArray
  }
  
}
