package snakefish.alt
package cipher.classical

import PolybiusSquare._

class ADFGVX (square: PolybiusSquare, transpositionKey: CharSequence, strictMode: Boolean = false)
    extends { override protected val cipherChars = "ADFGVX" }
    with ADFGX(square, transpositionKey, strictMode)

object ADFGVX {
  
  def apply(square: PolybiusSquare, transpositionKey: CharSequence, strictMode: Boolean = false) = 
    new ADFGVX(square, transpositionKey, strictMode)
}
