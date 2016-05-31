package snakefish.crypto
package data

class AlphabetTest extends BaseTest {
  
  private val plainAlphabet = "абвгдежзийклмн"
  
  ".apply" should "create Alphabet instance from string" in {
    val alphabet = Alphabet(plainAlphabet)
    alphabet.toString must be (plainAlphabet)
  }
  
  ".apply" should "remove duplicate characters from alphabet string" in {
    val alphabet = Alphabet("аббвгдеежзиайкллллмнмн")
    alphabet.toString must be (plainAlphabet)
  }
  
  ".length" should "return correct length of Alphabet" in {
    val alphabet = Alphabet(plainAlphabet)
    alphabet.length must be (plainAlphabet.length)
  }
  
  ".+(string)" should "concatenate string with existing Alphabet" in {
    val alphabet = Alphabet(plainAlphabet) + "йклмнщшцщшъ"
    alphabet.toString must be (plainAlphabet + "щшцъ")
  }
  
  ".+(Alphabet)" should "concatenate 2 Alphabets" in {
    val alphabet = Alphabet(plainAlphabet) + Alphabet("йклмнщшцщшъ")
    alphabet.toString must be (plainAlphabet + "щшцъ")
  }
  
}
