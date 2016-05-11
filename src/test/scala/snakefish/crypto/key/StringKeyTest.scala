package snakefish.crypto.key

import snakefish.crypto.BaseTest
import snakefish.crypto.utils.FileUtils
import java.io.File

class StringKeyTest extends BaseTest {
  
  private val plainKey = "Ехал грека через реку, видит грека - в реке рак..."
  
  ".apply" should "create StringKey instance from string" in {
    val key = StringKey(plainKey)
    key.toString must be (plainKey)
  }
  
  ".fromFile(filePath)" should "read key in default UTF-8 charset from file, specified by path" in {
    FileUtils.writeString(plainKey, TEST_FILE)
    val key = StringKey.fromFile(TEST_FILE)
    key.toString must be (plainKey)
  }
  
  ".fromFile(filePath, charset)" should "read key in non-default charset from file, specified by path" in {
    FileUtils.writeString(plainKey, TEST_FILE2, NON_DEFAULT_CHARSET)
    val key = StringKey.fromFile(TEST_FILE2, NON_DEFAULT_CHARSET)
    key.toString must be (plainKey)
  }
  
  ".fromFile(file)" should "read key in default UTF-8 charset from file" in {
    FileUtils.writeString(plainKey, TEST_FILE)
    val key = StringKey.fromFile(new File(TEST_FILE))
    key.toString must be (plainKey)
  }
  
  ".fromFile(file, charset)" should "read key in non-default charset from file" in {
    FileUtils.writeString(plainKey, TEST_FILE2, NON_DEFAULT_CHARSET)
    val key = StringKey.fromFile(new File(TEST_FILE2), NON_DEFAULT_CHARSET)
    key.toString must be (plainKey)
  }
  
}
