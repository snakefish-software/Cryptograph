package snakefish.crypto
package cipher.historical

import utils.CryptoUtils._

object Atbash {
  
  def compute(data: CharSequence, alphabet: String, strictMode: Boolean = false) = {
    sumKeySeqWithText(i => i)(data, 
                              alphabet, 
                              strictMode, 
                              (dataChIndex, _, alphabetLen) => alphabetLen - dataChIndex - 1);
  }
  
}
