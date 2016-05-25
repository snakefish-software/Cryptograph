package snakefish.crypto

class packageTest extends BaseTest {
  
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
  
}