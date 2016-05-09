package snakefish.crypto.data

case class NotInAlphabetException() extends Exception("Data contains symbols that are missing in provided alphabet")
