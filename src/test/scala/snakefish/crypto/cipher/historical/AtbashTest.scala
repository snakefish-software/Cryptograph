package snakefish.crypto
package cipher.historical

class AtbashTest extends BaseTest {
  
  ".compute" should "correctly compute result according to Atbash rules" in {
    val alphabetEn = Alphabet.ENGLISH.toString;
    val resultEn = Atbash.compute(alphabetEn, Alphabet.ENGLISH)
    resultEn must be (alphabetEn.reverse.toCharArray)
    
    val alphabetRu = Alphabet.RUSSIAN.toString;
    val resultRu = Atbash.compute(alphabetRu, Alphabet.RUSSIAN)
    resultRu must be (alphabetRu.reverse.toCharArray)
  }
  
  ".compute" should "throw an exception in strict mode if income data contains symbols that are missing in alphabet" in {
    an [DataCharNotInAlphabetException] should be thrownBy Atbash.compute(" ", Alphabet.ENGLISH, true)
  }
  
}
