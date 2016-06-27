package snakefish.crypto
package cipher.classical

class AtbashTest extends BaseTest {
  
  ".crypt" must "correctly compute result according to Atbash rules" in {
    val alphabetEn = Alphabet.ENGLISH.toString;
    val resultEn = Atbash(Alphabet.ENGLISH).crypt(alphabetEn)
    resultEn must be (alphabetEn.reverse)
    
    val alphabetRu = Alphabet.RUSSIAN.toString;
    val resultRu = Atbash(Alphabet.RUSSIAN).crypt(alphabetRu)
    resultRu must be (alphabetRu.reverse)
  }
  
  ".crypt(strictMode)" must "throw an exception if data contains symbols that are missing in alphabet" in {
    val ex = the [DataCharNotInAlphabetException] thrownBy Atbash(Alphabet.ENGLISH, true).crypt("abc ")
    ex.position must be (3)
  }
  
}
