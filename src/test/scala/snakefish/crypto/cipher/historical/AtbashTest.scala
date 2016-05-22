package snakefish.crypto.cipher.historical

import snakefish.crypto.BaseTest
import snakefish.crypto.data.Alphabet
import snakefish.crypto.data.DataCharNotInAlphabetException

class AtbashTest extends BaseTest {
  
  ".compute" should "correctly compute result according to Atbash rules" in {
    val alphabetEn = Alphabet.ENGLISH.toString;
    val resultEn = Atbash.compute(alphabetEn, alphabetEn)
    resultEn must be (alphabetEn.reverse.toCharArray)
    
    val alphabetRu = Alphabet.RUSSIAN.toString;
    val resultRu = Atbash.compute(alphabetRu, alphabetRu)
    resultRu must be (alphabetRu.reverse.toCharArray)
  }
  
  ".compute" should "throw an exception in strict mode if income data contains symbols that are missing in alphabet" in {
    an [DataCharNotInAlphabetException] should be thrownBy Atbash.compute(" ", Alphabet.ENGLISH, true)
  }
  
}
