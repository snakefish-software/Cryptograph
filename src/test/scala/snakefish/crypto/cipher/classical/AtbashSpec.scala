package snakefish.crypto
package cipher.classical

class AtbashSpec extends BaseSpec {
  
  ".crypt" must "correctly compute result according to Atbash rules" in {
    val alphabetEn = Alphabet.ENGLISH.toString;
    val resultEn = Atbash(Alphabet.ENGLISH).crypt(alphabetEn)
    resultEn must be (alphabetEn.reverse)
    
    val alphabetRu = Alphabet.RUSSIAN.toString;
    val resultRu = Atbash(Alphabet.RUSSIAN).crypt(alphabetRu)
    resultRu must be (alphabetRu.reverse)
    
    Atbash(Alphabet.ENGLISH).crypt("") must be ("")
  }
  
  it must "throw an exception in strict mode if data contains symbols that are missing in alphabet" in {
    val ex = the [DataCharNotInAlphabetException] thrownBy Atbash(Alphabet.ENGLISH, true).crypt("abc ")
    ex.char must be (' ')
    ex.position must be (3)
  }
  
}
