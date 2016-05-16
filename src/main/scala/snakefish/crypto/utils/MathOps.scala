package snakefish.crypto.utils

object MathOps {
  
  def addByModulo(x: Int, y: Int, mod: Int) = (x % mod + y % mod) % mod

  def subtractByModulo(x: Int, y: Int, mod: Int) = (x % mod - y % mod + mod) % mod

}
