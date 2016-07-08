package snakefish.alt

import scala.collection.mutable.ArrayBuffer
import scala.language.higherKinds

trait EraseChar[T] {
  val value: T
}

trait Erase[E[_]] {
  def erase[A](x: E[A])(implicit e: EraseChar[A]): E[A]
}

object Erase {
  def apply[E[_]](implicit ev: Erase[E]) = ev
}

trait EraseInstances {
  implicit object eraseInt extends EraseChar[Int] { val value = 0}
  implicit object eraseByte extends EraseChar[Byte] { val value = 0: Byte}
  implicit object eraseChar extends EraseChar[Char] { val value = 0.toChar}

  implicit object ArrayErase extends Erase[Array] {
    def erase[A](x: Array[A])(implicit e: EraseChar[A]): Array[A] = {
      for (i <- 0 until x.length) x.update(i, e.value)
      x
    }
  }

  implicit object ArrayBufferErase extends Erase[ArrayBuffer] {
    def erase[A](x: ArrayBuffer[A])(implicit e: EraseChar[A]): ArrayBuffer[A] = {
      for (i <- 0 until x.length) x.update(i, e.value)
      x
    }
  }
}
