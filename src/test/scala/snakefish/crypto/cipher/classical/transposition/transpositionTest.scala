package snakefish.crypto
package cipher.classical
package transposition

class transpositionTest extends BaseTest {
  
  ".normalizeKey" must "create correct 0-based key for transpostion ciphers" in {
    normalizeKey(
        Array(25, 4, 2, 25, 13, 0, 15, 0, 2), true) must equal (
        Array( 7, 4, 2,  8,  5, 0,  6, 1, 3))
    
    normalizeKey(
        Array(25, 4, 2, 25, 13, 0, 15, 0, 2), false) must equal (
        Array( 5, 2, 1,  5,  3, 0,  4, 0, 1))
  }
  
}
