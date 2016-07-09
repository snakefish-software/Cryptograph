package snakefish.crypto
package cipher.classical

object Atbash {
  
  def apply(alphabet: Alphabet, strictMode: Boolean = false) = 
    new Atbash(alphabet, strictMode)
}

class Atbash(val alphabet: Alphabet, val strictMode: Boolean = false) {

  @throws(classOf[DataCharNotInAlphabetException])
  def crypt(data: CharSequence): String = {
    sumKeySeqWithText(identity)(data,
                                alphabet,
                                strictMode,
                                (dataCharIndex, _, alphabetLength) => alphabetLength - dataCharIndex - 1)
  }

}
