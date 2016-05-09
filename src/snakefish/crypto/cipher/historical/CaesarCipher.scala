package snakefish.crypto.cipher.historical

import snakefish.crypto.cipher.Cipher
import snakefish.crypto.cipher.CipherOptions
import snakefish.crypto.data.StringData
import snakefish.crypto.key.NumberKey

case class Options (val useStrictMode: Boolean) extends CipherOptions

object CaesarCipher extends Cipher[StringData, NumberKey, Options] {
  
  def encode(data: StringData, key: NumberKey, options: Options = null) = {
    // TODO: Dima Siryk: implement this
  }
  
  def decode(data: StringData, key: NumberKey, options: Options = null) = {
    // TODO: Dima Siryk: implement this
  }
  
}
