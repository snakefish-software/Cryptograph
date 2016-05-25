package snakefish.crypto
package cipher.historical

object Atbash {

  def compute(data: CharSequence, alphabet: String, strictMode: Boolean = false) = {
    sumKeySeqWithText(identity)(data,
                                alphabet,
                                strictMode,
                                (dataChIndex, _, alphabetLen) => alphabetLen - dataChIndex - 1);
  }

}
