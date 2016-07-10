package snakefish.crypto

class AlphabetSpec extends BaseSpec {
  
  private val plainAlphabet = "абвгдежзийклмн"
  
  "KeyCharNotInAlphabetException" must "have correct exception message" in {
    val ex = new KeyCharNotInAlphabetException('a', 5)
    ex.getMessage must be ("Char 'a' at position 5 is missing in alphabet")
  }
  
  "DataCharNotInAlphabetException" must "have correct exception message" in {
    val ex = new DataCharNotInAlphabetException('a', 5)
    ex.getMessage must be ("Char 'a' at position 5 is missing in alphabet")
  }
  
  ".apply" must "create Alphabet instance from string" in {
    val alphabet = Alphabet(plainAlphabet)
    alphabet.toString must be (plainAlphabet)
  }
  
  it must "remove duplicate characters from alphabet string" in {
    val alphabet = Alphabet("аббвгдеежзиайкллллмнмн")
    alphabet.toString must be (plainAlphabet)
  }
  
  ".length" must "return correct length of Alphabet" in {
    val alphabet = Alphabet(plainAlphabet)
    alphabet.length must be (plainAlphabet.length)
  }
  
  ".+" must "concatenate string with existing Alphabet" in {
    val alphabet = Alphabet(plainAlphabet) + "йклмнщшцщшъ"
    alphabet.toString must be (plainAlphabet + "щшцъ")
  }
  
  it must "concatenate 2 Alphabets" in {
    val alphabet = Alphabet(plainAlphabet) + Alphabet("йклмнщшцщшъ")
    alphabet.toString must be (plainAlphabet + "щшцъ")
  }
  
}
