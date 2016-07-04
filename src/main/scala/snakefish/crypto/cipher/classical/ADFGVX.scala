package snakefish.crypto
package cipher.classical

import PolybiusSquare._

object ADFGVX {
  
  @throws(classOf[WrongSquareSizeException])
  def apply(square: PolybiusSquare, transpositionKey: CharSequence, strictMode: Boolean = false) = 
    new ADFGVX(square, transpositionKey, strictMode)
}

class ADFGVX(square: PolybiusSquare, transpositionKey: CharSequence, strictMode: Boolean = false)
    extends { override protected val cipherChars = "ADFGVX" }
    with ADFGX(square, transpositionKey, strictMode)
