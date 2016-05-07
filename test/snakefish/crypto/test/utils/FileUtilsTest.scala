package snakefish.crypto.test.utils

import snakefish.crypto.utils.FileUtils
import snakefish.crypto.test.BaseTest

class FileUtilsTest extends BaseTest {
  
  ".writeNumber/.readNumber" should "correctly write/read single number to/from file" in {
    val savedNumber = 1234567890L
    FileUtils.writeNumber(savedNumber, TEST_FILE)
    val readNumber = FileUtils.readNumber(TEST_FILE)
    readNumber must be (savedNumber)
  }
  
  ".writeString/.readString" should "correctly write/read string to/from file in non-default charset" in {
    val savedString = "Беги, Лола, беги"
    FileUtils.writeString(savedString, TEST_FILE, NON_DEFAULT_CHARSET)
    val readString = FileUtils.readString(TEST_FILE, NON_DEFAULT_CHARSET)
    readString must be (savedString)
  }
  
}
