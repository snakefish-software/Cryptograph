package snakefish.crypto

class AlphabetTest extends BaseTest {
  
  private val plainAlphabet = "абвгдежзийклмн"
  
  "KeyCharNotInAlphabetException" must "have correct exception message" in {
    val ex = new KeyCharNotInAlphabetException(5)
    ex.getMessage must be ("Key char at position 5 is missing in alphabet")
  }
  
  "DataCharNotInAlphabetException" must "have correct exception message" in {
    val ex = new DataCharNotInAlphabetException(5)
    ex.getMessage must be ("Data char at position 5 is missing in alphabet")
  }
  
  ".apply" must "create Alphabet instance from string" in {
    val alphabet = Alphabet(plainAlphabet)
    alphabet.toString must be (plainAlphabet)
  }
  
  ".apply" must "remove duplicate characters from alphabet string" in {
    val alphabet = Alphabet("аббвгдеежзиайкллллмнмн")
    alphabet.toString must be (plainAlphabet)
  }
  
  ".length" must "return correct length of Alphabet" in {
    val alphabet = Alphabet(plainAlphabet)
    alphabet.length must be (plainAlphabet.length)
  }
  
  ".+(string)" must "concatenate string with existing Alphabet" in {
    val alphabet = Alphabet(plainAlphabet) + "йклмнщшцщшъ"
    alphabet.toString must be (plainAlphabet + "щшцъ")
  }
  
  ".+(Alphabet)" must "concatenate 2 Alphabets" in {
    val alphabet = Alphabet(plainAlphabet) + Alphabet("йклмнщшцщшъ")
    alphabet.toString must be (plainAlphabet + "щшцъ")
  }
  
}
