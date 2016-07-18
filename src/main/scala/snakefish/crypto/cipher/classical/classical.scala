package snakefish.crypto
package cipher

import scala.collection.mutable.ArrayBuffer
import java.util.BitSet

package object classical {
  
  case class OddCiphertextLengthException()
      extends RuntimeException("Ciphertext length must be even")
  
  case class PlaceholderNotInSquareException()
      extends RuntimeException("Placeholder char is missing in Polybius square")
  
  @throws(classOf[DataCharNotInAlphabetException])
  def sumKeySeqWithText(keyProvider: Int => Int)(
    data: CharSequence,
    alphabet: Alphabet,
    strictMode: Boolean,
    resultIndexCalc: (Int, Int, Int) => Int
  ): String = {
    val result = new StringBuilder(data.length)
    var calcIndex = 0
    
    for (i <- 0 until data.length) {
      val dataChar = data.charAt(i)
      val charIndex = alphabet.indexOf(dataChar)
      if (charIndex >= 0) {
        val resultIndex = resultIndexCalc(charIndex, keyProvider(calcIndex), alphabet.length)
        result += { if (dataChar.isUpper) alphabet(resultIndex).toUpper else alphabet(resultIndex) }
        calcIndex += 1
      } else {
        if (strictMode) {
          throw new DataCharNotInAlphabetException(dataChar, i)
        } else result += dataChar
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
    resultIndexCalc: (Int, Int, Int) => Int
  ): String = {
    val keyLength = key.length
    val keyF = { i: Int => if (keyLength > 0) key(i % keyLength) else 0 }
    sumKeySeqWithText(keyF)(data, alphabet, strictMode, resultIndexCalc)
  }
  
  def indicesInAlphabet(chars: CharSequence, alphabet: Alphabet): Array[Int] = {
    val indices = new ArrayBuffer[Int](chars.length)
    for (i <- 0 until chars.length) {
      val charIndex = alphabet.indexOf(chars.charAt(i))
      if (charIndex >= 0)
        indices += charIndex
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
    val random = getRandomInstance()
    random.setSeed(key)
    val dataLength = data.length
    val result = new Array[Char](dataLength)
    
    for (i <- 0 until dataLength) {
      val pos = random.nextInt(dataLength - i)
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
    val rand = getRandomInstance()
    rand.setSeed(key)
    val dataLength = data.length
    val result = new Array[Char](dataLength)
    val alreadyRead = new BitSet(dataLength)
    
    for (i <- 0 until dataLength) {
      val pos = rand.nextInt(dataLength - i)
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
