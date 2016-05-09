package snakefish.crypto.test.data

import snakefish.crypto.test.BaseTest
import snakefish.crypto.data.Alphabet
import snakefish.crypto.utils.FileUtils
import java.io.File

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
  
  ".fromFile(filePath)" should "read alphabet in default UTF-8 charset from file, specified by path" in {
    FileUtils.writeString(plainAlphabet, TEST_FILE)
    val alphabet = Alphabet.fromFile(TEST_FILE)
    alphabet.toString must be (plainAlphabet)
  }
  
  ".fromFile(filePath, charset)" should "read alphabet in non-default charset from file, specified by path" in {
    FileUtils.writeString(plainAlphabet, TEST_FILE2, NON_DEFAULT_CHARSET)
    val alphabet = Alphabet.fromFile(TEST_FILE2, NON_DEFAULT_CHARSET)
    alphabet.toString must be (plainAlphabet)
  }
  
  ".fromFile(file)" should "read alphabet in default UTF-8 charset from file" in {
    FileUtils.writeString(plainAlphabet, TEST_FILE)
    val alphabet = Alphabet.fromFile(new File(TEST_FILE))
    alphabet.toString must be (plainAlphabet)
  }
  
  ".fromFile(file, charset)" should "read alphabet in non-default charset from file" in {
    FileUtils.writeString(plainAlphabet, TEST_FILE2, NON_DEFAULT_CHARSET)
    val alphabet = Alphabet.fromFile(new File(TEST_FILE2), NON_DEFAULT_CHARSET)
    alphabet.toString must be (plainAlphabet)
  }
  
}
