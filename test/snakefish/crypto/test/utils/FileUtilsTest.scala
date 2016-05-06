package snakefish.crypto.test.utils

import snakefish.crypto.test._
import snakefish.crypto.utils.FileUtils;

import org.junit.Assert._
import org.junit.Test
import org.junit.After

import java.io.File

class FileUtilsTest extends BaseTest {
  
  @Test
  def testReadWriteNumber() {
    val savedNum = 123456789L
    FileUtils.writeNumber(savedNum, TEST_FILE_PATH)
    val readNum = FileUtils.readNumber(TEST_FILE_PATH)
    assertEquals(savedNum, readNum)
  }
  
  @Test
  def testReadWriteString_WithCharset() {
    val savedString = "Беги, Лола, беги"
    FileUtils.writeString(savedString, TEST_FILE_PATH, "KOI8_R")
    val readString = FileUtils.readString(TEST_FILE_PATH, "KOI8_R")
    assertEquals(savedString, readString)
  }
  
}