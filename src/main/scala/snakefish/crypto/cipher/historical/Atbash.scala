package snakefish.crypto
package cipher.historical

object Atbash {
  def apply(alphabet: Alphabet, strictMode: Boolean = false) = 
    new Atbash(alphabet, strictMode)
}

class Atbash(val alphabet: Alphabet, val strictMode: Boolean = false) {

  @throws(classOf[DataCharNotInAlphabetException])
  def compute(data: CharSequence): Array[Char] = {
    sumKeySeqWithText(identity)(data,
                                alphabet,
                                strictMode,
                                (dataChIndex, _, alphabetLen) => alphabetLen - dataChIndex - 1);
  }

}
