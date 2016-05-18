package snakefish.crypto.cipher.historical

import snakefish.crypto.BaseTest
import snakefish.crypto.data.Alphabet

class AtbashTest extends BaseTest {
  
  ".compute" should "correctly compute result according to Atbash rules" in {
    val alphabetEn = Alphabet.ENGLISH.toString;
    val resultEn = Atbash.compute(alphabetEn, alphabetEn)
    resultEn must be (alphabetEn.reverse.toCharArray)
    
    val alphabetRu = Alphabet.RUSSIAN.toString;
    val resultRu = Atbash.compute(alphabetRu, alphabetRu)
    resultRu must be (alphabetRu.reverse.toCharArray)
  }
  
}
