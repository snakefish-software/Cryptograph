package snakefish.crypto
package cipher.classical

class classicalSpec extends BaseSpec {
  
  private val plainText = "The technique encrypts pairs of letters"
  private val shuffledText1 = "ethyer recept e ufhiqT spl neanttosrsci"
  private val shuffleKey1 = 123456789
  private val shuffledText2 = "icfrtlrysehhu itepasq  eoeeTe crpnt tsn"
  private val shuffleKey2 = 987654321

  ".toDigits" must "return array of digits that make up a number" in {
    val res1 = toDigits(1234567890)
    res1 must be (Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 0))
    
    val res2 = toDigits(3)
    res2 must be (Array(3))
  }
  
  ".shuffle" must "shuffle char sequence using provided key" in {
    val shuffled1 = shuffle(shuffleKey1, plainText)
    shuffled1 must be (shuffledText1.toCharArray)
    
    val shuffled2 = shuffle(shuffleKey2, plainText)
    shuffled2 must be (shuffledText2.toCharArray)
  }
  
  ".deshuffle" must "reverse the result of .shuffle()" in {
    val plain1 = deshuffle(shuffleKey1, shuffledText1)
    plain1 must be (plainText.toCharArray)
    
    val plain2 = deshuffle(shuffleKey2, shuffledText2)
    plain2 must be (plainText.toCharArray)
  }
  
}
