package snakefish.crypto
package cipher.symmetric

class XORSpec extends BaseSpec {
  
  private val bytePlaintext  = Array(byte("01010111"), byte("01101001"), byte("01101011"), byte("01101001"))
  private val byteKey        = Array(byte("11110011"))
  private val byteCiphertext = Array(byte("10100100"), byte("10011010"), byte("10011000"), byte("10011010"))
  
  private val charPlaintext  = Array(char("01010111"), char("01101001"), char("01101011"), char("01101001"))
  private val charKey        = Array(char("11110011"))
  private val charCiphertext = Array(char("10100100"), char("10011010"), char("10011000"), char("10011010"))
  
  ".crypt" must "correctly XOR 2 byte arrays" in {
    XOR.crypt(byteKey, bytePlaintext) must be (byteCiphertext)
    XOR.crypt(byteKey, byteCiphertext) must be (bytePlaintext)
  }
  
  it must "correctly XOR 2 char sequences" in {
    XOR.crypt(charKey, charPlaintext) must be (charCiphertext)
    XOR.crypt(charKey, charCiphertext) must be (charPlaintext)
  }
  
}
