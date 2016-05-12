package snakefish.crypto.utils

import snakefish.crypto.BaseTest

class MathOpsTest extends BaseTest {
  
  ".addByModulo" should "correctly add 2 numbers by modulo" in {
    MathOps.addByModulo(12, 3, 11) must be (4)
    MathOps.addByModulo(12, 10, 11) must be (0)
  }
  
  ".subtractByModulo" should "correctly subtract 2 numbers by modulo" in {
    MathOps.subtractByModulo(12, 3, 11) must be (9)
    MathOps.subtractByModulo(12, 20, 11) must be (3)
    MathOps.subtractByModulo(12, 1, 11) must be (0)
  }
  
}
