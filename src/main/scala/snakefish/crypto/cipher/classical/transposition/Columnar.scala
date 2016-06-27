package snakefish.crypto
package cipher.classical
package transposition

object Columnar {
  
  def apply(key: CharSequence, alphabet: Alphabet): Columnar = 
    apply(indicesInAlphabet(key, alphabet))
  
  def apply(key: Array[Int]): Columnar = 
    new Columnar(key)
  
}

class Columnar(private val _key: Array[Int]) {
  
  val key = normalizeKey(_key, true)
  
  def encrypt(plaintext: CharSequence): String = {
    val colsCount = key.length
    val cols = new Array[StringBuilder](colsCount)
    for (i <- 0 until colsCount) {
      cols(i) = new StringBuilder()
    }
    
    for (i <- 0 until plaintext.length) {
      cols(i % colsCount) += plaintext.charAt(i)
    }
    
    val result = new StringBuilder(plaintext.length)
    for (i <- 0 until colsCount) {
      result append cols(key.indexOf(i))
    }
    
    result.toString
  }
  
  def decrypt(ciphertext: CharSequence): String = {
    ""
  }
  
}
