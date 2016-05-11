package snakefish.crypto.cipher

import snakefish.crypto.data.Data
import snakefish.crypto.key.Key

trait Cipher[D <: Data, K <: Key, O <: CipherOptions] {
  
  def encode(data: D, key: K, options: O)
    
  def decode(data: D, key: K, options: O)
  
}
