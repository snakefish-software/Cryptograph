package snakefish.crypto
package cipher.historical

import scala.collection.mutable.ArrayBuffer

object Vigenere {
  def apply(alphabet: Alphabet, strictMode: Boolean = false) = 
    new Vigenere(alphabet, strictMode)
}

class Vigenere(val alphabet: Alphabet, val strictMode: Boolean = false) {

  @throws(classOf[DataCharNotInAlphabetException])
  def encode(data: CharSequence, key: CharSequence): Array[Char] = 
    cryptoFunc(data, key)(addByModulo)
  
  @throws(classOf[DataCharNotInAlphabetException])
  def encode(data: CharSequence, key: Array[Int]): Array[Char] = 
    sumKeySeqWithText(data, key, alphabet, strictMode)(addByModulo)

  @throws(classOf[DataCharNotInAlphabetException])
  def decode(data: CharSequence, key: CharSequence): Array[Char] = 
    cryptoFunc(data, key)(subtractByModulo)
  
  @throws(classOf[DataCharNotInAlphabetException])
  def decode(data: CharSequence, key: Array[Int]): Array[Char] = 
    sumKeySeqWithText(data, key, alphabet, strictMode)(subtractByModulo)

  private def cryptoFunc(
    data: CharSequence,
    key: CharSequence
  )(
    resIndexCalc: (Int, Int, Int) => Int
  ): Array[Char] = {
    val keyInts = new ArrayBuffer[Int](key.length)
    for (i <- 0 until key.length) {
      val keyChIndex = alphabet.indexOf(key.charAt(i))
      if (keyChIndex >= 0)
        keyInts += keyChIndex
    }
    sumKeySeqWithText(data, keyInts.toArray, alphabet, strictMode)(resIndexCalc)
  }

}
