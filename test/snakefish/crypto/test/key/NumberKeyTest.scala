package snakefish.crypto.test.key

import snakefish.crypto.key.NumberKey
import snakefish.crypto.utils.FileUtils
import snakefish.crypto.test.BaseTest
import java.io.File

class NumberKeyTest extends BaseTest {
  
  private val plainKey = 1234567890L
  
  ".apply" should "create Alphabet instance from string" in {
    val key = NumberKey(plainKey)
    key.toNumber must be (plainKey)
  }
  
  ".fromFile(filePath)" should "read key from file, specified by path" in {
    FileUtils.writeNumber(plainKey, TEST_FILE)
    val key = NumberKey.fromFile(TEST_FILE)
    key.toNumber must be (plainKey)
  }
  
  ".fromFile(file)" should "read key from file" in {
    FileUtils.writeNumber(plainKey, TEST_FILE)
    val key = NumberKey.fromFile(new File(TEST_FILE))
    key.toNumber must be (plainKey)
  }
  
}
