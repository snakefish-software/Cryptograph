package snakefish.crypto
package cipher.actual.symmetric

object XOR {
  
  def compute(data: Array[Byte], key: Array[Byte]): Array[Byte] = 
    xor(data, key)
  
  def compute(data: CharSequence, key: CharSequence): Array[Char] = 
    xor(data, key)
  
}
