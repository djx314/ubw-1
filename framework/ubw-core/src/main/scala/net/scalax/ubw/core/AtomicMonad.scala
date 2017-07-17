package net.scalax.ubw.core

import net.scalax.fsn.core.{FAtomicPathImpl, FAtomicValueImpl}

trait AtomicMonad[U] {

  def transform[T, F[_]](path: FAtomicPathImpl[T], value: FAtomicValueImpl[T]): F[(FAtomicValueImpl[T], U)]

  def nextPile[R, F[_]](oldWrap: F[(FAtomicValueImpl[R], U)], value: FAtomicValueImpl[R]): F[(FAtomicValueImpl[R], U)]

}