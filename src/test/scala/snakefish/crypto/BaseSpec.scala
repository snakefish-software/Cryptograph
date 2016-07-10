package snakefish.crypto

import org.scalatest.FlatSpec
import org.scalatest.MustMatchers

trait BaseSpec extends FlatSpec with MustMatchers {
  
  def byte(binaryStr: String) = Integer.parseInt(binaryStr, 2).toByte
  
  def char(binaryStr: String) = Integer.parseInt(binaryStr, 2).toChar
  
}
