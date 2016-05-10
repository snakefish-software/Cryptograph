package snakefish.crypto.data

import snakefish.crypto.BaseTest
import snakefish.crypto.utils.FileUtils
import java.io.File

class StringDataTest extends BaseTest {
  
  private val plainData = "Ехал грека через реку, видит грека - в реке рак..."
  
  def iteratedData(data: StringData) = {
    val iteratedData = new StringBuilder
    while (data.hasNext) {
      iteratedData.append(data.next())
    }
    iteratedData.toString
  }
  
  ".apply" should "create StringData instance from string" in {
    val data = StringData(plainData)
    data.toString must be (plainData)
  }
  
  ".hasNext and .next" should "correctly iterate over same string data 2 times" in {
    val data = StringData(plainData)
    iteratedData(data) must be (plainData)
    iteratedData(data) must be (plainData)
  }
  
  ".hasNext and .next" should "correctly iterate over same file data 2 times" in {
    FileUtils.writeString(plainData, TEST_FILE)
    val data = StringData.fromFile(TEST_FILE)
    iteratedData(data) must be (plainData)
    iteratedData(data) must be (plainData)
  }
  
  ".fromFile(filePath)" should "read data in default UTF-8 charset from file, specified by path" in {
    FileUtils.writeString(plainData, TEST_FILE2)
    val data = StringData.fromFile(TEST_FILE2)
    iteratedData(data) must be (plainData)
  }
  
  ".fromFile(filePath, charset)" should "read data in non-default charset from file, specified by path" in {
    FileUtils.writeString(plainData, TEST_FILE, NON_DEFAULT_CHARSET)
    val data = StringData.fromFile(TEST_FILE, NON_DEFAULT_CHARSET)
    iteratedData(data) must be (plainData)
  }
  
  ".fromFile(file)" should "read data in default UTF-8 charset from file" in {
    FileUtils.writeString(plainData, TEST_FILE2)
    val data = StringData.fromFile(new File(TEST_FILE2))
    iteratedData(data) must be (plainData)
  }
  
  ".fromFile(file, charset)" should "read data in non-default charset from file" in {
    FileUtils.writeString(plainData, TEST_FILE, NON_DEFAULT_CHARSET)
    val data = StringData.fromFile(new File(TEST_FILE), NON_DEFAULT_CHARSET)
    iteratedData(data) must be (plainData)
  }
  
}
