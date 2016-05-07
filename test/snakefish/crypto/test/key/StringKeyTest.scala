package snakefish.crypto.test.key

import snakefish.crypto.test.BaseTest
import snakefish.crypto.key.StringKey
import snakefish.crypto.utils.FileUtils
import snakefish.crypto.key.NumberKey
import java.io.File

class StringKeyTest extends BaseTest {
  
  private val plainKey = "Ехал грека через реку, видит грека - в реке рак..."
  
  ".from" should "create StringKey instance from string" in {
    val key = StringKey.from(plainKey)
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
  
  ".toFile(filePath)" should "save key in default UTF-8 charset to file, specified by path" in {
    StringKey.from(plainKey).toFile(TEST_FILE)
    val readKey = FileUtils.readString(TEST_FILE)
    readKey must be (plainKey)
  }
  
  ".toFile(filePath, charset)" should "save key in non-default charset to file, specified by path" in {
    StringKey.from(plainKey).toFile(TEST_FILE2, NON_DEFAULT_CHARSET)
    val readKey = FileUtils.readString(TEST_FILE2, NON_DEFAULT_CHARSET)
    readKey must be (plainKey)
  }
  
  ".toFile(file)" should "save key in default UTF-8 charset to file" in {
    StringKey.from(plainKey).toFile(new File(TEST_FILE))
    val readKey = FileUtils.readString(TEST_FILE)
    readKey must be (plainKey)
  }
  
  ".toFile(file, charset)" should "save key in non-default charset to file" in {
    StringKey.from(plainKey).toFile(new File(TEST_FILE2), NON_DEFAULT_CHARSET)
    val readKey = FileUtils.readString(TEST_FILE2, NON_DEFAULT_CHARSET)
    readKey must be (plainKey)
  }
  
}
