package snakefish.crypto
package cipher.historical

import PolybiusSquare._

class PolybiusSquareTest extends BaseTest {
  
  "KeyCharNotInSquareException" should "have correct exception message" in {
    val ex = new KeyCharNotInSquareException(5)
    ex.getMessage must be ("Key char at position 5 is missing in Polybius square")
  }
  
  "DataCharNotInSquareException" should "have correct exception message" in {
    val ex = new DataCharNotInSquareException(5)
    ex.getMessage must be ("Data char at position 5 is missing in Polybius square")
  }
  
  "CoordinatesOutOfBoundsException" should "have correct exception message" in {
    val ex = new CoordinatesOutOfBoundsException(5, -3, 100)
    ex.getMessage must be ("Coordinates (row = -3; column = 100) of char at position 5 are out of Polybius square bounds")
  }
  
  ".apply(key: CharSequence, alphabet, missedOnExisting)" should "create Polibius square instance from provided arguments" in {
    val square = PolybiusSquare("QwErTy", Alphabet.ENGLISH, Map('J' -> 'I'))
    square.square must equal(Array(Array('q', 'w', 'e', 'r', 't'), 
                                   Array('y', 'a', 'b', 'c', 'd'),
                                   Array('f', 'g', 'h', 'i', 'k'),
                                   Array('l', 'm', 'n', 'o', 'p'),
                                   Array('s', 'u', 'v', 'x', 'z')))
    square.missedToExisting must contain only(('j' -> 'i'))
  }
  
  ".apply(key: Long, alphabet, missedOnExisting)" should "create Polibius square instance from provided arguments" in {
    val square = PolybiusSquare(123456789, Alphabet.ENGLISH, Map('J' -> 'I'))
    square.square must equal(Array(Array('l', 's', 'r', 'k', 'p'),
                                   Array('q', 'a', 'o', 'g', 'x'),
                                   Array('u', 't', 'd', 'f', 'y'),
                                   Array('b', 'n', 'i', 'w', 'c'),
                                   Array('e', 'h', 'm', 'v', 'z')))
    square.missedToExisting must contain only(('j' -> 'i'))
  }
  
  ".lowerSymbol" should "correctly compute result" in {
    val res1 = compute("AgN tZj12", LATIN, lowerSymbol)
    res1 must be ("FmS yEo12".toCharArray)
    
    val res2 = compute("АжН фЫя", RUSSIAN_ALL, lowerSymbol)
    res2 must be ("ЁмУ ъДв".toCharArray)
    
    val res3 = compute("АзПцЮёЙъ", RUSSIAN_SHORT, lowerSymbol)
    res3 must be ("ЖоХэДмПв".toCharArray)
  }
  
  ".upperSymbol" should "correctly compute result" in {
    val res1 = compute("FmSyEo", LATIN, upperSymbol)
    res1 must be ("AgNtZi".toCharArray)
    
    val res2 = compute("ЁмУъДв", RUSSIAN_ALL, upperSymbol)
    res2 must be ("АжНфЫя".toCharArray)
    
    val res3 = compute("ЖоХэДмПв", RUSSIAN_SHORT, upperSymbol)
    res3 must be ("АзПцЮеИь".toCharArray)
  }
  
  ".rowsCols" should "correctly compute result" in {
    val res1 = compute("AgNtZ", LATIN, rowsCols)
    res1 must be ("BoVhU".toCharArray)
    
    val res2 = compute("АжНфЫя", RUSSIAN_ALL, rowsCols)
    res2 must be ("БоЬбОщ".toCharArray)
    
    val res3 = compute("АзПцЮёЙъ", RUSSIAN_SHORT, rowsCols)
    res3 must be ("БрЩлБрЯп".toCharArray)
  }
  
  ".rowsColsReverse" should "correctly compute result" in {
    val res1 = compute("BoVhU", LATIN, rowsColsReverse)
    res1 must be ("AgNtZ".toCharArray)
    
    val res2 = compute("БоЬбОщ", RUSSIAN_ALL, rowsColsReverse)
    res2 must be ("АжНфЫя".toCharArray)
    
    val res3 = compute("БрЩлБрЯп", RUSSIAN_SHORT, rowsColsReverse)
    res3 must be ("АзПцЮеИь".toCharArray)
  }
  
  ".colsRows" should "correctly compute result" in {
    val res1 = compute("AgNtZ", LATIN, colsRows)
    res1 must be ("BoVhU".toCharArray)
    
    val res2 = compute("АжНфЫя", RUSSIAN_ALL, colsRows)
    res2 must be ("БоЩбОь".toCharArray)
    
    val res3 = compute("АзПцЮёЙъ", RUSSIAN_SHORT, colsRows)
    res3 must be ("БрЯпБрЩл".toCharArray)
  }
  
  ".colsRowsReverse" should "correctly compute result" in {
    val res1 = compute("BoVhU", LATIN, colsRowsReverse)
    res1 must be ("AgNtZ".toCharArray)
    
    val res2 = compute("БоЩбОь", RUSSIAN_ALL, colsRowsReverse)
    res2 must be ("АжНфЫя".toCharArray)
    
    val res3 = compute("БрЯпБрЩл", RUSSIAN_SHORT, colsRowsReverse)
    res3 must be ("АзПцЮеИь".toCharArray)
  }
  
  ".compute(strictMode)" should "throw an exception if symbol in income data is missing in square" in {
    val ex = the [DataCharNotInSquareException] thrownBy compute("F12yEo", LATIN, upperSymbol, true)
    ex.position must be (1)
  }
  
  ".compute" should "throw an exception if symbol in computed data is missing in square" in {
    val ex = the [CoordinatesOutOfBoundsException] thrownBy compute("ц", RUSSIAN_ALL, colsRows)
    ex.position must be (0)
    ex.row must be (5)
    ex.col must be (3)
  }
  
}
