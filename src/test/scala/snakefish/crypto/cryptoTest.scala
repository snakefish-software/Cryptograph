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
  
  ".xor(Byte, Byte)" should "correctly XOR 2 bytes" in {
    xor(byte("01010111"),
        byte("11110011")) must be 
       (byte("10100100"))
  }
  
  ".xor(Char, Char)" should "correctly XOR 2 chars" in {
    xor(char("01010111"),
        char("11110011")) must be 
       (char("10100100"))
  }
  
  ".xor(Array[Byte], Array[Byte])" should "correctly XOR 2 byte arrays" in {
    val data = Array(byte("01010111"), byte("01101001"), byte("01101011"), byte("01101001"))
    val key  = Array(byte("11110011"))
    val exp  = Array(byte("10100100"), byte("10011010"), byte("10011000"), byte("10011010"))
    xor(data, key) must be (exp)
  }
  
  ".xor(CharSequence, CharSequence)" should "correctly XOR 2 char sequences" in {
    val data = Array(char("10100100"), char("10011010"), char("10011000"), char("10011010"))
    val key  = Array(char("11110011"))
    val exp  = Array(char("01010111"), char("01101001"), char("01101011"), char("01101001"))
    xor(data, key) must be (exp)
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
