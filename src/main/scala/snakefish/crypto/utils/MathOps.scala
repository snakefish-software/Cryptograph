package snakefish.crypto.utils

object MathOps {
  
  def addByModulo(summand1: Int, summand2: Int, modulo: Int) = {
    ((summand1 % modulo) + (summand2 % modulo)) % modulo
  }
  
  def subtractByModulo(minuend: Int, subtrahend: Int, modulo: Int) = {
    ((minuend % modulo) - (subtrahend % modulo) + modulo) % modulo
  }
  
}
