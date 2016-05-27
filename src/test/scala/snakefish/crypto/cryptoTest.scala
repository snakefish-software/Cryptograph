package snakefish.crypto

class cryptoTest extends BaseTest {
  
  private val plainText = "The technique encrypts pairs of letters"
  private val shuffledText1 = "ethyer recept e ufhiqT spl neanttosrsci"
  private val shuffleKey1 = 123456789
  private val shuffledText2 = "icfrtlrysehhu itepasq  eoeeTe crpnt tsn"
  private val shuffleKey2 = 987654321
  
  ".addByModulo" should "correctly add 2 numbers by modulo" in {
    addByModulo(12, 3, 11) must be (4)
    addByModulo(12, 10, 11) must be (0)
  }
  
  ".subtractByModulo" should "correctly subtract 2 numbers by modulo" in {
    subtractByModulo(12, 3, 11) must be (9)
    subtractByModulo(12, 20, 11) must be (3)
    subtractByModulo(12, 1, 11) must be (0)
  }
  
  ".toDigits" should "return array of digits that make up a number" in {
    val res1 = toDigits(1234567890)
    res1 must be (Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 0))
    
    val res2 = toDigits(3)
    res2 must be (Array(3))
  }
  
  ".shuffle" should "shuffle char sequence using provided key" in {
    val shuffled1 = shuffle(plainText, shuffleKey1)
    shuffled1 must be (shuffledText1.toCharArray)
    
    val shuffled2 = shuffle(plainText, shuffleKey2)
    shuffled2 must be (shuffledText2.toCharArray)
  }
  
  ".deshuffle" should "reverse result of .shuffle()" in {
    val plain1 = deshuffle(shuffledText1, shuffleKey1)
    plain1 must be (plainText.toCharArray)
    
    val plain2 = deshuffle(shuffledText2, shuffleKey2)
    plain2 must be (plainText.toCharArray)
  }
  
}
