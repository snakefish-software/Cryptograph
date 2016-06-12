package snakefish.crypto.cipher

package object historical {
  
  class OddCifertextLengthException
      extends RuntimeException("Cifertext length must be even")
  
  class PlaceholderNotInSquareException
      extends RuntimeException("Placeholder char is missing in Polybius square")
  
}