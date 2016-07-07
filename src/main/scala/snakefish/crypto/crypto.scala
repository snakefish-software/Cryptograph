package snakefish

import scala.language.higherKinds
import java.security.SecureRandom

package object crypto extends EraseInstances {

  def addByModulo(x: Int, y: Int, mod: Int): Int = (x % mod + y % mod) % mod

  def subtractByModulo(x: Int, y: Int, mod: Int): Int = (x % mod - y % mod + mod) % mod
  
  def xor(b1: Byte, b2: Byte): Byte = (b1 ^ b2).toByte
  
  def xor(ch1: Char, ch2: Char): Char = (ch1 ^ ch2).toChar
  
  def xor(key: Array[Byte], data: Array[Byte]): Array[Byte] = {
    val result = new Array[Byte](data.length)
    val keyLen = key.length
    for (i <- 0 until data.length) {
      result(i) = xor(data(i), key(i % keyLen))
    }
    result
  }
  
  def xor(key: CharSequence, data: CharSequence): Array[Char] = {
    val result = new Array[Char](data.length)
    val keyLen = key.length
    for (i <- 0 until data.length) {
      val dataCh = data.charAt(i)
      val keyCh = key.charAt(i % keyLen)
      result(i) = xor(dataCh, keyCh)
    }
    result
  }
  
  def erase[R, E[R]](x: E[R])(implicit ev: Erase[E], evCh: EraseChar[R]): E[R] =
    ev.erase(x)(evCh)
    
  def secureRandom(): SecureRandom = 
    SecureRandom.getInstance("SHA1PRNG")

}
