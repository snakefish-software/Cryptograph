package snakefish.crypto.cipher.historical

import snakefish.crypto.BaseTest
import PolybiusSquare._

class AdfgxTest extends BaseTest {

  private val square = PolybiusSquare(
    Array(
      Array('F', 'N', 'H', 'E', 'Q'),
      Array('R', 'D', 'Z', 'O', 'C'),
      Array('I', 'S', 'A', 'G', 'U'),
      Array('B', 'V', 'K', 'P', 'W'),
      Array('X', 'M', 'Y', 'T', 'L')
    ),
    Map('J' -> 'I')
  )
  val cipherText = "FFFFFFFFGFDDXGDAXDXGGXGX"

  ".encode" should "correctly encode data using provided parameters" in {
    val encoded = Adfgx.encode("ATTACK AT DAWN", "BATTLE", square)
    encoded must be (cipherText)
  }

  ".encode" should "correctly handle blocks with different length" in {

    val sq = PolybiusSquare(
      Array(
        Array('B', 'T', 'A', 'L', 'P'),
        Array('D', 'H', 'O', 'Z', 'K'),
        Array('Q', 'F', 'V', 'S', 'N'),
        Array('G', 'J', 'C', 'U', 'X'),
        Array('M', 'R', 'E', 'W', 'Y')
      ),
      Map('J' -> 'I')
    )

    Adfgx.encode("ATTACK AT ONCE", "CARGO", sq) must be ("FAXDFADDDGDGFFFAFAXAFAFX")
  }

  ".decode" should "correctly decode data using provided parameters" in {
    val plainText = Adfgx.decode(cipherText, "BATTLE", square)
    plainText must be ("ATTACKATDAWN")
  }
  
}
