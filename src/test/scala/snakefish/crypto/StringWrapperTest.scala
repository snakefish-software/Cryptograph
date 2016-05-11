package snakefish.crypto

import snakefish.crypto.utils.FileUtils
import java.io.File

class StringWrapperTest extends BaseTest {
  
  private val plainVal = "Я сажаю алюминиевые огурцы на брезентовом поле, оу-ооо!!! :)"
  private val wrapper = new StringWrapperImpl
  
  class StringWrapperImpl extends StringWrapper {
    override def toString = plainVal
  }
  
  ".toFile(filePath)" should "save wrapped string in default UTF-8 charset to file, specified by path" in {
    wrapper.toFile(TEST_FILE)
    val readVal = FileUtils.readString(TEST_FILE)
    readVal must be (plainVal)
  }
  
  ".toFile(filePath, charset)" should "save wrapped string in non-default charset to file, specified by path" in {
    wrapper.toFile(TEST_FILE2, NON_DEFAULT_CHARSET)
    val readVal = FileUtils.readString(TEST_FILE2, NON_DEFAULT_CHARSET)
    readVal must be (plainVal)
  }
  
  ".toFile(file)" should "save wrapped string in default UTF-8 charset to file" in {
    wrapper.toFile(new File(TEST_FILE))
    val readVal = FileUtils.readString(TEST_FILE)
    readVal must be (plainVal)
  }
  
  ".toFile(file, charset)" should "save wrapped string in non-default charset to file" in {
    wrapper.toFile(new File(TEST_FILE2), NON_DEFAULT_CHARSET)
    val readVal = FileUtils.readString(TEST_FILE2, NON_DEFAULT_CHARSET)
    readVal must be (plainVal)
  }
  
}
