package snakefish.crypto
package cipher.actual.symmetric

import OneTimePad._

class OneTimePadTest extends BaseTest {
  
  private val bytePlaintext = Array(byte("01010111"), byte("01101001"), byte("01101011"), byte("01101001"))
  private val byteKey       = Array(byte("11110011"), byte("11110011"), byte("11110011"), byte("11110011"), byte("11110011"))
  private val byteCifertext = Array(byte("10100100"), byte("10011010"), byte("10011000"), byte("10011010"))
  
  private val charPlaintext = "attackatdawn"
  private val charKey       = "lemonlemonlemon"
  private val charCifertext = "lxfopvefrnhr"
  
  ".compute(Array[Byte], Array[Byte])" must "correctly XOR 2 byte arrays" in {
    OneTimePad.compute(bytePlaintext, byteKey) must be (byteCifertext)
    OneTimePad.compute(byteCifertext, byteKey) must be (bytePlaintext)
  }
  
  ".compute(Array[Byte], Array[Byte])" must "throw an exception if key length < data length" in {
    an [KeyLengthInsuffisientException] must be thrownBy OneTimePad.compute(bytePlaintext, Array(byte("11110011")))
  }
  
  ".encode(CharSequence, CharSequence, Alphabet)" must "correctly encode data using provided key and alphabet" in {
    val _charCifertext = OneTimePad.encode(charPlaintext, charKey, Alphabet.ENGLISH)
    _charCifertext must be (charCifertext.toCharArray)
  }
  
  ".decode(CharSequence, CharSequence, Alphabet)" must "correctly decode data using provided key and alphabet" in {
    val _charPlaintext = OneTimePad.decode(charCifertext, charKey, Alphabet.ENGLISH)
    _charPlaintext must be (charPlaintext.toCharArray)
  }
  
  ".encode(CharSequence, CharSequence, Alphabet)" must "throw an exception if key length < data length" in {
    an [KeyLengthInsuffisientException] must be thrownBy OneTimePad.encode(charPlaintext, "lemon", Alphabet.ENGLISH)
  }
  
  ".decode(CharSequence, CharSequence, Alphabet)" must "throw an exception if key length < data length" in {
    an [KeyLengthInsuffisientException] must be thrownBy OneTimePad.decode(charCifertext, "lemon", Alphabet.ENGLISH)
  }
  
  ".encode(CharSequence, CharSequence, Alphabet)" must "throw an exception if data contains char that is missing in alphabet" in {
    val ex = the [DataCharNotInAlphabetException] thrownBy OneTimePad.encode(" " + charPlaintext, charKey, Alphabet.ENGLISH)
    ex.position must be (0)
  }
  
  ".decode(CharSequence, CharSequence, Alphabet)" must "throw an exception if data contains char that is missing in alphabet" in {
    val ex = the [DataCharNotInAlphabetException] thrownBy OneTimePad.decode(" " + charCifertext, charKey, Alphabet.ENGLISH)
    ex.position must be (0)
  }
  
  ".encode(CharSequence, CharSequence, Alphabet)" must "throw an exception if key contains char that is missing in alphabet" in {
    val ex = the [KeyCharNotInAlphabetException] thrownBy OneTimePad.encode(charPlaintext, " " + charKey, Alphabet.ENGLISH)
    ex.position must be (0)
  }
  
  ".decode(CharSequence, CharSequence, Alphabet)" must "throw an exception if key contains char that is missing in alphabet" in {
    val ex = the [KeyCharNotInAlphabetException] thrownBy OneTimePad.decode(charCifertext, " " + charKey, Alphabet.ENGLISH)
    ex.position must be (0)
  }
  
}
