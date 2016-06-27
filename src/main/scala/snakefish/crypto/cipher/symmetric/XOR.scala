package snakefish.crypto
package cipher.symmetric

object XOR {
  
  def crypt(key: Array[Byte], data: Array[Byte]): Array[Byte] = 
    xor(key, data)
  
  def crypt(key: CharSequence, data: CharSequence): Array[Char] = 
    xor(key, data)
  
}
