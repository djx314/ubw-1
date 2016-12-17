package net.scalax.fsn.core

trait FZeroPile[T] {

  def zero: T

}

object FZeroPile {
  implicit def optionPileZero[T]: FZeroPile[Option[T]] = new FZeroPile[Option[T]] {
    override def zero = None
  }
}
