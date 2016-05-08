package snakefish.crypto.test

import snakefish.crypto.utils.FileUtils
import snakefish.crypto.NumberWrapper
import snakefish.crypto.key.NumberKey
import java.io.File

class NumberWrapperTest extends BaseTest {
  
  private val plainVal = 1234567890L
  private val wrapper = new NumberWrapperImpl
  
  class NumberWrapperImpl extends NumberWrapper {
    def toNumber = plainVal
  }
  
  ".toFile(filePath)" should "save wrapped number to file, specified by path" in {
    wrapper.toFile(TEST_FILE)
    val readVal = FileUtils.readNumber(TEST_FILE)
    readVal must be (plainVal)
  }
  
  ".toFile(file)" should "save wrapped number to file" in {
    wrapper.toFile(new File(TEST_FILE))
    val readVal = FileUtils.readNumber(TEST_FILE)
    readVal must be (plainVal)
  }
  
}
