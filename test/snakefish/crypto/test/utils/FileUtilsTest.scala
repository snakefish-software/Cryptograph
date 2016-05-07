package snakefish.crypto.test.utils

import snakefish.crypto.utils.FileUtils
import snakefish.crypto.test.BaseTest

class FileUtilsTest extends BaseTest {
  
  ".writeNumber/.readNumber" should "correctly write/read single number to/from file" in {
    val savedNumber = 1234567890L
    FileUtils.writeNumber(savedNumber, TEST_FILE_PATH)
    val readNumber = FileUtils.readNumber(TEST_FILE_PATH)
    readNumber must be (savedNumber)
  }
  
  ".writeString/.readString" should "correctly write/read string to/from file in non UTF-8 charset" in {
    val savedString = "Беги, Лола, беги"
    FileUtils.writeString(savedString, TEST_FILE_PATH, "KOI8_R")
    val readString = FileUtils.readString(TEST_FILE_PATH, "KOI8_R")
    readString must be (savedString)
  }
  
}
