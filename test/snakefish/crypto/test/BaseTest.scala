package snakefish.crypto.test

import org.junit.After
import java.io.File

class BaseTest {
  
  @After
  def deleteTestFiles() {
    val file = new File(TEST_FILE_PATH)
    file.delete()
  }
  
}