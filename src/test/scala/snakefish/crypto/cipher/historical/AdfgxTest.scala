package snakefish.crypto.cipher.historical

import snakefish.crypto.BaseTest
import PolybiusSquare._

class AdfgxTest extends BaseTest {

  "Adfgx" should "encode" in {

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

  it should "encode if key has repeated chars" in {
    val square = PolybiusSquare(
      Array(
        Array('F', 'N', 'H', 'E', 'Q'),
        Array('R', 'D', 'Z', 'O', 'C'),
        Array('I', 'S', 'A', 'G', 'U'),
        Array('B', 'V', 'K', 'P', 'W'),
        Array('X', 'M', 'Y', 'T', 'L')
      ),
      Map('J' -> 'I')
    )

    Adfgx.encode("ATTACK AT DAWN", "BATTLE", square) must be ("FFFFFFFFGFDDXGDAXDXGGXGX")
  }

  it should "decode" in {
    val square = PolybiusSquare(
      Array(
        Array('F', 'N', 'H', 'E', 'Q'),
        Array('R', 'D', 'Z', 'O', 'C'),
        Array('I', 'S', 'A', 'G', 'U'),
        Array('B', 'V', 'K', 'P', 'W'),
        Array('X', 'M', 'Y', 'T', 'L')
      ),
      Map('J' -> 'I')
    )

    Adfgx.decode("FFFFFFFFGFDDXGDAXDXGGXGX", "BATTLE", square) must be ("ATTACKATDAWN")
  }

  "Adfgvx" should "encode" in {
    val sq = PolybiusSquare(
      Array(
        Array('1', 'G', 'R', '4', 'H', 'D'),
        Array('E', '2', 'A', 'V', '9', 'M'),
        Array('8', 'P', 'I', 'N', 'K', 'Z'),
        Array('B', 'Y', 'U', 'F', '6', 'T'),
        Array('5', 'G', 'X', 'S', '3', 'O'),
        Array('W', 'L', 'Q', '7', 'C', '0')
      ))

    val encoded = Adfgvx.encode("ATTACK WILL BEGIN IN 11 AM", "SECRET", sq)
    encoded must be ("GXFGFFDFFADDFAGFXDFADXVFAFGFDDXXVFAXVDAGAX")
  }

  it should "decode" in {
    val sq = PolybiusSquare(
      Array(
        Array('1', 'G', 'R', '4', 'H', 'D'),
        Array('E', '2', 'A', 'V', '9', 'M'),
        Array('8', 'P', 'I', 'N', 'K', 'Z'),
        Array('B', 'Y', 'U', 'F', '6', 'T'),
        Array('5', 'G', 'X', 'S', '3', 'O'),
        Array('W', 'L', 'Q', '7', 'C', '0')
      ))

    val plainText = Adfgvx.decode("GXFGFFDFFADDFAGFXDFADXVFAFGFDDXXVFAXVDAGAX", "SECRET", sq)
    plainText must be ("ATTACKWILLBEGININ11AM")
  }

}
