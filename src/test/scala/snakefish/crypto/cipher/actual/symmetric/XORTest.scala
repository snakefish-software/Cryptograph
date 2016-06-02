package snakefish.crypto
package cipher.actual.symmetric

class XORTest extends BaseTest {
  
  private val bytePlaintext = Array(byte("01010111"), byte("01101001"), byte("01101011"), byte("01101001"))
  private val byteKey       = Array(byte("11110011"))
  private val byteCifertext = Array(byte("10100100"), byte("10011010"), byte("10011000"), byte("10011010"))
  
  private val charPlaintext = Array(char("01010111"), char("01101001"), char("01101011"), char("01101001"))
  private val charKey       = Array(char("11110011"))
  private val charCifertext = Array(char("10100100"), char("10011010"), char("10011000"), char("10011010"))
  
  ".compute(Array[Byte], Array[Byte])" should "correctly XOR 2 byte arrays" in {
    XOR.compute(bytePlaintext, byteKey) must be (byteCifertext)
    XOR.compute(byteCifertext, byteKey) must be (bytePlaintext)
  }
  
  ".compute(CharSequence, CharSequence)" should "correctly XOR 2 char sequences" in {
    XOR.compute(charPlaintext, charKey) must be (charCifertext)
    XOR.compute(charCifertext, charKey) must be (charPlaintext)
  }
  
}
