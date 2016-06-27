package snakefish.crypto
package cipher.symmetric

import OneTimePad._

class OneTimePadTest extends BaseTest {
  
  private val bytePlaintext  = Array(byte("01010111"), byte("01101001"), byte("01101011"), byte("01101001"))
  private val byteKey        = Array(byte("11110011"), byte("11110011"), byte("11110011"), byte("11110011"), byte("11110011"))
  private val byteCiphertext = Array(byte("10100100"), byte("10011010"), byte("10011000"), byte("10011010"))
  
  private val charPlaintext  = "attackatdawn"
  private val charKey        = "lemonlemonlemon"
  private val charCiphertext = "lxfopvefrnhr"
  
  private val cipher = OneTimePad(Alphabet.ENGLISH)
  
  ".crypt" must "correctly XOR 2 byte arrays" in {
    OneTimePad.crypt(byteKey, bytePlaintext) must be (byteCiphertext)
    OneTimePad.crypt(byteKey, byteCiphertext) must be (bytePlaintext)
  }
  
  it must "throw an exception if key length < data length" in {
    an [KeyLengthInsuffisientException] must be thrownBy OneTimePad.crypt(Array(byte("11110011")), bytePlaintext)
  }
  
  ".encrypt" must "correctly encrypt plaintext" in {
    val _charCiphertext = cipher.encrypt(charKey, charPlaintext)
    _charCiphertext must be (charCiphertext.toCharArray)
  }
  
  it must "throw an exception if key length < data length" in {
    an [KeyLengthInsuffisientException] must be thrownBy cipher.encrypt("lemon", charPlaintext)
  }
  
  it must "throw an exception if plaintext contains char that is missing in alphabet" in {
    val ex = the [DataCharNotInAlphabetException] thrownBy cipher.encrypt(charKey, " " + charPlaintext)
    ex.position must be (0)
  }
  
  it must "throw an exception if key contains char that is missing in alphabet" in {
    val ex = the [KeyCharNotInAlphabetException] thrownBy cipher.encrypt(" " + charKey, charPlaintext)
    ex.position must be (0)
  }
  
  ".decrypt" must "correctly decrypt ciphertext" in {
    val _charPlaintext = cipher.decrypt(charKey, charCiphertext)
    _charPlaintext must be (charPlaintext.toCharArray)
  }
  
  it must "throw an exception if key length < data length" in {
    an [KeyLengthInsuffisientException] must be thrownBy cipher.decrypt("lemon", charCiphertext)
  }
  
  it must "throw an exception if ciphertext contains char that is missing in alphabet" in {
    val ex = the [DataCharNotInAlphabetException] thrownBy cipher.decrypt(charKey, " " + charCiphertext)
    ex.position must be (0)
  }
  
  it must "throw an exception if key contains char that is missing in alphabet" in {
    val ex = the [KeyCharNotInAlphabetException] thrownBy cipher.decrypt(" " + charKey, charCiphertext)
    ex.position must be (0)
  }
  
}
