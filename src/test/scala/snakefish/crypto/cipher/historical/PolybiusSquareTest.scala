package snakefish.crypto.cipher.historical

import snakefish.crypto.BaseTest
import PolybiusSquare.DataCharNotInSquareException

class PolybiusSquareTest extends BaseTest {
  
  ".lowerSymbol" should "correctly compute result" in {
    val res1 = PolybiusSquare.compute("AgN tZj12", PolybiusSquare.LATIN, PolybiusSquare.lowerSymbol)
    res1 must be ("FmS yEo12".toCharArray)
    
    val res2 = PolybiusSquare.compute("АжН фЫя", PolybiusSquare.RUSSIAN_ALL, PolybiusSquare.lowerSymbol)
    res2 must be ("ЁмУ ъДв".toCharArray)
    
    val res3 = PolybiusSquare.compute("АзПцЮёЙъ", PolybiusSquare.RUSSIAN_SHORT, PolybiusSquare.lowerSymbol)
    res3 must be ("ЖоХэДмПв".toCharArray)
  }
  
  ".upperSymbol" should "correctly compute result" in {
    val res1 = PolybiusSquare.compute("FmSyEo", PolybiusSquare.LATIN, PolybiusSquare.upperSymbol)
    res1 must be ("AgNtZi".toCharArray)
    
    val res2 = PolybiusSquare.compute("ЁмУъДв", PolybiusSquare.RUSSIAN_ALL, PolybiusSquare.upperSymbol)
    res2 must be ("АжНфЫя".toCharArray)
    
    val res3 = PolybiusSquare.compute("ЖоХэДмПв", PolybiusSquare.RUSSIAN_SHORT, PolybiusSquare.upperSymbol)
    res3 must be ("АзПцЮеИь".toCharArray)
  }
  
  ".rowsCols" should "correctly compute result" in {
    val res1 = PolybiusSquare.compute("AgNtZ", PolybiusSquare.LATIN, PolybiusSquare.rowsCols)
    res1 must be ("BoVhU".toCharArray)
    
    val res2 = PolybiusSquare.compute("АжНфЫя", PolybiusSquare.RUSSIAN_ALL, PolybiusSquare.rowsCols)
    res2 must be ("БоЬбОщ".toCharArray)
    
    val res3 = PolybiusSquare.compute("АзПцЮёЙъ", PolybiusSquare.RUSSIAN_SHORT, PolybiusSquare.rowsCols)
    res3 must be ("БрЩлБрЯп".toCharArray)
  }
  
  ".rowsColsReverse" should "correctly compute result" in {
    val res1 = PolybiusSquare.compute("BoVhU", PolybiusSquare.LATIN, PolybiusSquare.rowsColsReverse)
    res1 must be ("AgNtZ".toCharArray)
    
    val res2 = PolybiusSquare.compute("БоЬбОщ", PolybiusSquare.RUSSIAN_ALL, PolybiusSquare.rowsColsReverse)
    res2 must be ("АжНфЫя".toCharArray)
    
    val res3 = PolybiusSquare.compute("БрЩлБрЯп", PolybiusSquare.RUSSIAN_SHORT, PolybiusSquare.rowsColsReverse)
    res3 must be ("АзПцЮеИь".toCharArray)
  }
  
  ".colsRows" should "correctly compute result" in {
    val res1 = PolybiusSquare.compute("AgNtZ", PolybiusSquare.LATIN, PolybiusSquare.colsRows)
    res1 must be ("BoVhU".toCharArray)
    
    val res2 = PolybiusSquare.compute("АжНфЫя", PolybiusSquare.RUSSIAN_ALL, PolybiusSquare.colsRows)
    res2 must be ("БоЩбОь".toCharArray)
    
    val res3 = PolybiusSquare.compute("АзПцЮёЙъ", PolybiusSquare.RUSSIAN_SHORT, PolybiusSquare.colsRows)
    res3 must be ("БрЯпБрЩл".toCharArray)
  }
  
  ".colsRowsReverse" should "correctly compute result" in {
    val res1 = PolybiusSquare.compute("BoVhU", PolybiusSquare.LATIN, PolybiusSquare.colsRowsReverse)
    res1 must be ("AgNtZ".toCharArray)
    
    val res2 = PolybiusSquare.compute("БоЩбОь", PolybiusSquare.RUSSIAN_ALL, PolybiusSquare.colsRowsReverse)
    res2 must be ("АжНфЫя".toCharArray)
    
    val res3 = PolybiusSquare.compute("БрЯпБрЩл", PolybiusSquare.RUSSIAN_SHORT, PolybiusSquare.colsRowsReverse)
    res3 must be ("АзПцЮеИь".toCharArray)
  }
  
  ".compute" should "throw an exception in strict mode if symbol in income data is missing in square" in {
    an [DataCharNotInSquareException] should be thrownBy PolybiusSquare.compute("F12yEo", PolybiusSquare.LATIN, PolybiusSquare.upperSymbol, true)
  }
  
  ".compute" should "throw an exception if symbol in computed data is missing in square" in {
    an [DataCharNotInSquareException] should be thrownBy PolybiusSquare.compute("ц", PolybiusSquare.RUSSIAN_ALL, PolybiusSquare.colsRows)
  }
  
}
