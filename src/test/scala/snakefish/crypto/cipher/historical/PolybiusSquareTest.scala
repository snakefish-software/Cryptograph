package snakefish.crypto.cipher.historical

import snakefish.crypto.BaseTest

class PolybiusSquareTest extends BaseTest {
  
  ".compute" should "correctly compute result with '.lowerSymbol' method" in {
    val res = PolybiusSquare.compute("SOMETEXT", PolybiusSquare.LATIN, PolybiusSquare.lowerSymbol)
    res must be ("XTRKYKCY".toCharArray)
  }
  
}
