package snakefish.crypto.utils

import snakefish.crypto.BaseTest

class CryptoUtilsTest extends BaseTest {
  
  ".toDigits" should "return array of digits that make up a number" in {
    val res1 = CryptoUtils.toDigits(1234567890)
    res1 must be (Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 0))
    
    val res2 = CryptoUtils.toDigits(3)
    res2 must be (Array(3))
  }
  
}
