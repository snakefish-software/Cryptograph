package snakefish.crypto.test

import java.io.File
import org.scalatest.FlatSpec
import org.scalatest.MustMatchers
import org.scalatest.BeforeAndAfterEach
import org.scalatest.BeforeAndAfter

class BaseTest extends FlatSpec with MustMatchers with BeforeAndAfter {
  
  val TEST_FILE_PATH = """D:\test"""
  
  after {
    new File(TEST_FILE_PATH).delete()
  }
  
}
