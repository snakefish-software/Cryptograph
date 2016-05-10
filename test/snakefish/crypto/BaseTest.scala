package snakefish.crypto

import java.io.File
import org.scalatest.FlatSpec
import org.scalatest.MustMatchers
import org.scalatest.BeforeAndAfter

class BaseTest extends FlatSpec with MustMatchers with BeforeAndAfter {
  
  val TEST_FILE = "test"
  val TEST_FILE2 = "test2"
  val NON_DEFAULT_CHARSET = "KOI8_R"
  
  after {
    new File(TEST_FILE).delete()
    new File(TEST_FILE2).delete()
  }
  
}
