package snakefish.alt

import org.scalatest.{FlatSpecLike, MustMatchers}

trait BaseSpec extends FlatSpecLike with MustMatchers {
  
  def byte(binaryStr: String) = Integer.parseInt(binaryStr, 2).toByte
  
  def char(binaryStr: String) = Integer.parseInt(binaryStr, 2).toChar
  
}
