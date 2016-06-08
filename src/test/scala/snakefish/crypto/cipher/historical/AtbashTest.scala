package snakefish.crypto
package cipher.historical

class AtbashTest extends BaseTest {
  
  ".compute" must "correctly compute result according to Atbash rules" in {
    val alphabetEn = Alphabet.ENGLISH.toString;
    val resultEn = Atbash.compute(alphabetEn, Alphabet.ENGLISH)
    resultEn must be (alphabetEn.reverse.toCharArray)
    
    val alphabetRu = Alphabet.RUSSIAN.toString;
    val resultRu = Atbash.compute(alphabetRu, Alphabet.RUSSIAN)
    resultRu must be (alphabetRu.reverse.toCharArray)
  }
  
  ".compute(strictMode)" must "throw an exception if income data contains symbols that are missing in alphabet" in {
    val ex = the [DataCharNotInAlphabetException] thrownBy Atbash.compute("abc ", Alphabet.ENGLISH, true)
    ex.position must be (3)
  }
  
}
