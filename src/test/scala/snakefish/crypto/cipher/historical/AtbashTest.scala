package snakefish.crypto
package cipher.historical

class AtbashTest extends BaseTest {
  
  ".compute" must "correctly compute result according to Atbash rules" in {
    val alphabetEn = Alphabet.ENGLISH.toString;
    val resultEn = Atbash(Alphabet.ENGLISH).compute(alphabetEn)
    resultEn must be (alphabetEn.reverse.toCharArray)
    
    val alphabetRu = Alphabet.RUSSIAN.toString;
    val resultRu = Atbash(Alphabet.RUSSIAN).compute(alphabetRu)
    resultRu must be (alphabetRu.reverse.toCharArray)
  }
  
  ".compute(strictMode)" must "throw an exception if income data contains symbols that are missing in alphabet" in {
    val ex = the [DataCharNotInAlphabetException] thrownBy Atbash(Alphabet.ENGLISH, true).compute("abc ")
    ex.position must be (3)
  }
  
}
