package snakefish.crypto.test.key

import snakefish.crypto.key.NumberKey
import snakefish.crypto.utils.FileUtils
import snakefish.crypto.test.BaseTest

class NumberKeyTest extends BaseTest {
  
  private val plainKey = 1234567890L
  
  ".from" should "create NumberKey instance from number" in {
    val key = NumberKey.from(plainKey)
    key.toNumber must be (plainKey)
  }
  
  ".fromFile" should "read key from file" in {
    FileUtils.writeNumber(plainKey, TEST_FILE)
    val key = NumberKey.fromFile(TEST_FILE)
    key.toNumber must be (plainKey)
  }
  
  ".toFile" should "save key to file" in {
    val key = NumberKey.from(plainKey)
    key.toFile(TEST_FILE)
    val readKey = FileUtils.readNumber(TEST_FILE)
    readKey must be (plainKey)
  }
  
}
