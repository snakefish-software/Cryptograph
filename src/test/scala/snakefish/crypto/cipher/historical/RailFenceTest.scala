package snakefish.crypto
package cipher.historical

import RailFence._

class RailFenceTest extends BaseTest {
  
  private val plaintext  = "abcdefghijklmnopqr"
  private val ciphertext = "aiqbhjprcgkodflnem"
  private val cipher = RailFence(5)
  
  ".apply" must "throw an exception if rows count is incorrect" in {
    an [IllegalRowsCountException] must be thrownBy RailFence(1)
  }
  
  ".encrypt" must "correctly encrypt plaintext" in {
    val _ciphertext = cipher.encrypt(plaintext)
    _ciphertext must be (ciphertext)
  }
  
  ".decrypt" must "correctly decrypt ciphertext" in {
    val _plaintext = cipher.decrypt(ciphertext)
    _plaintext must be (plaintext)
  }
  
}
