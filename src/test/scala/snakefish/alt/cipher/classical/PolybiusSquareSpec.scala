package snakefish.alt
package cipher.classical

import PolybiusSquare._

class PolybiusSquareSpec extends BaseSpec {

  "KeyCharNotInSquareException" must "have correct exception message" in {
    val ex = new KeyCharNotInSquareException(5)
    ex.getMessage must be ("Key char at position 5 is missing in Polybius square")
  }
  
  "DataCharNotInSquareException" must "have correct exception message" in {
    val ex = new DataCharNotInSquareException(5)
    ex.getMessage must be ("Data char at position 5 is missing in Polybius square")
  }
  
  "CoordinatesOutOfBoundsException" must "have correct exception message" in {
    val ex = new CoordinatesOutOfBoundsException(5, -3, 100)
    ex.getMessage must be ("Coordinates (row = -3; column = 100) of char at position 5 are out of Polybius square bounds")
  }
  
  ".apply(key: CharSequence, alphabet, missedOnExisting)" must "create Polibius square instance from provided arguments" in {
    var square = PolybiusSquare("QwErTy", Alphabet.ENGLISH, Map('J' -> 'I'))
    square.row(0) must equal(Array('q', 'w', 'e', 'r', 't'))
    square.row(1) must equal(Array('y', 'a', 'b', 'c', 'd'))
    square.row(2) must equal(Array('f', 'g', 'h', 'i', 'k'))
    square.row(3) must equal(Array('l', 'm', 'n', 'o', 'p'))
    square.row(4) must equal(Array('s', 'u', 'v', 'x', 'z'))
    square.coords('j') must equal(square.coords('i'))

    square = PolybiusSquare(123456789, Alphabet.ENGLISH, Map('J' -> 'I'))
    square.row(0) must equal(Array('l', 's', 'r', 'k', 'p'))
    square.row(1) must equal(Array('q', 'a', 'o', 'g', 'x'))
    square.row(2) must equal(Array('u', 't', 'd', 'f', 'y'))
    square.row(3) must equal(Array('b', 'n', 'i', 'w', 'c'))
    square.row(4) must equal(Array('e', 'h', 'm', 'v', 'z'))
    square.coords('j') must equal(square.coords('i'))
  }
  
  ".lowerSymbol" must "correctly compute result" in {
    val res1 = compute("AgN tZj12", LATIN, lowerSymbol)
    res1 must be ("FmS yEo12")
    
    val res2 = compute("АжН фЫя", RUSSIAN_ALL, lowerSymbol)
    res2 must be ("ЁмУ ъДв")
    
    val res3 = compute("АзПцЮёЙъ", RUSSIAN_SHORT, lowerSymbol)
    res3 must be ("ЖоХэДмПв")
  }
  
  ".upperSymbol" must "correctly compute result" in {
    val res1 = compute("FmSyEo", LATIN, upperSymbol)
    res1 must be ("AgNtZi")
    
    val res2 = compute("ЁмУъДв", RUSSIAN_ALL, upperSymbol)
    res2 must be ("АжНфЫя")
    
    val res3 = compute("ЖоХэДмПв", RUSSIAN_SHORT, upperSymbol)
    res3 must be ("АзПцЮеИь")
  }
  
  ".rowsCols" must "correctly compute result" in {
    val res1 = compute("AgNtZ", LATIN, rowsCols)
    res1 must be ("BoVhU")
    
    val res2 = compute("АжНфЫя", RUSSIAN_ALL, rowsCols)
    res2 must be ("БоЬбОщ")
    
    val res3 = compute("АзПцЮёЙъ", RUSSIAN_SHORT, rowsCols)
    res3 must be ("БрЩлБрЯп")
  }
  
  ".rowsColsReverse" must "correctly compute result" in {
    val res1 = compute("BoVhU", LATIN, rowsColsReverse)
    res1 must be ("AgNtZ")
    
    val res2 = compute("БоЬбОщ", RUSSIAN_ALL, rowsColsReverse)
    res2 must be ("АжНфЫя")
    
    val res3 = compute("БрЩлБрЯп", RUSSIAN_SHORT, rowsColsReverse)
    res3 must be ("АзПцЮеИь")
  }
  
  ".colsRows" must "correctly compute result" in {
    val res1 = compute("AgNtZ", LATIN, colsRows)
    res1 must be ("BoVhU")
    
    val res2 = compute("АжНфЫя", RUSSIAN_ALL, colsRows)
    res2 must be ("БоЩбОь")
    
    val res3 = compute("АзПцЮёЙъ", RUSSIAN_SHORT, colsRows)
    res3 must be ("БрЯпБрЩл")
  }
  
  ".colsRowsReverse" must "correctly compute result" in {
    val res1 = compute("BoVhU", LATIN, colsRowsReverse)
    res1 must be ("AgNtZ")
    
    val res2 = compute("БоЩбОь", RUSSIAN_ALL, colsRowsReverse)
    res2 must be ("АжНфЫя")
    
    val res3 = compute("БрЯпБрЩл", RUSSIAN_SHORT, colsRowsReverse)
    res3 must be ("АзПцЮеИь")
  }
  
  ".compute" must "throw an exception if symbol in computed data is missing in square" in {
    val ex = the [CoordinatesOutOfBoundsException] thrownBy compute("ц", RUSSIAN_ALL, colsRows)
    ex.position must be (0)
    ex.row must be (5)
    ex.col must be (3)
  }

  it must "throw an exception in strict mode if symbol in input data is missing from square" in {
    val ex = the [DataCharNotInSquareException] thrownBy compute("F12yEo", LATIN.strict, upperSymbol, true)
  }
  
}
