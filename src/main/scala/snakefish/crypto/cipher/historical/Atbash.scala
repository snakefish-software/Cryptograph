package snakefish.crypto
package cipher.historical

object Atbash {

  def compute(data: CharSequence, alphabet: Alphabet, strictMode: Boolean = false): Array[Char] = {
    sumKeySeqWithText(identity)(data,
                                alphabet,
                                strictMode,
                                (dataChIndex, _, alphabetLen) => alphabetLen - dataChIndex - 1);
  }

}
