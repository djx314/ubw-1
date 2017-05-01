package net.scalax.fsn.core

import scala.language.higherKinds

trait FZeroPile[T[_]] {
  def zero[S]: T[S]
}

object FZeroPile {
  implicit def optionPileZero[T]: FZeroPile[Option] = new FZeroPile[Option] {
    override def zero[S] = None
  }
}
