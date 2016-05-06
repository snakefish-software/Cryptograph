package snakefish.crypto.test.key

import snakefish.crypto.test._
import org.junit.Assert._
import org.junit.Test
import org.junit.After
import java.io.File
import snakefish.crypto.key.NumberKey
import snakefish.crypto.utils.FileUtils

class NumberKeyTest extends BaseTest {
  
  private val plainKey = 123456789L
  
  @Test
  def testFrom() {
    val key = NumberKey.from(plainKey)
    assertEquals(plainKey, key.toNumber)
  }
  
  @Test
  def testFromFile() {
    FileUtils.writeNumber(plainKey, TEST_FILE_PATH)
    val key = NumberKey.fromFile(TEST_FILE_PATH)
    assertEquals(plainKey, key.toNumber)
  }
  
  @Test
  def testToFile() {
    val key = NumberKey.from(plainKey)
    key.toFile(TEST_FILE_PATH)
    val readKey = FileUtils.readNumber(TEST_FILE_PATH)
    assertEquals(plainKey, readKey)
  }
}