package snakefish.crypto

class cryptoTest extends BaseTest {
  
  ".addByModulo" must "correctly add 2 numbers by modulo" in {
    addByModulo(12, 3, 11) must be (4)
    addByModulo(12, 10, 11) must be (0)
  }
  
  ".subtractByModulo" must "correctly subtract 2 numbers by modulo" in {
    subtractByModulo(12, 3, 11) must be (9)
    subtractByModulo(12, 20, 11) must be (3)
    subtractByModulo(12, 1, 11) must be (0)
  }
  
  ".xor" must "correctly XOR 2 bytes" in {
    xor(byte("01010111"),
        byte("11110011")) must be 
       (byte("10100100"))
  }
  
  it must "correctly XOR 2 chars" in {
    xor(char("01010111"),
        char("11110011")) must be 
       (char("10100100"))
  }
  
  it must "correctly XOR 2 byte arrays" in {
    val data = Array(byte("01010111"), byte("01101001"), byte("01101011"), byte("01101001"))
    val key  = Array(byte("11110011"))
    val exp  = Array(byte("10100100"), byte("10011010"), byte("10011000"), byte("10011010"))
    xor(key, data) must be (exp)
  }
  
  it must "correctly XOR 2 char sequences" in {
    val data = Array(char("10100100"), char("10011010"), char("10011000"), char("10011010"))
    val key  = Array(char("11110011"))
    val exp  = Array(char("01010111"), char("01101001"), char("01101011"), char("01101001"))
    xor(key, data) must be (exp)
  }
  
}
