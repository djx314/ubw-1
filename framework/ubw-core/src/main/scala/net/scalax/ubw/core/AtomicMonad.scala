package net.scalax.ubw.core

import cats.{ Functor, Monad, Semigroup, Traverse }
import net.scalax.fsn.core.{ FAtomicValueImpl, FQueryTranform }
import cats.instances.list._

import scala.language.higherKinds

trait PileFilter[U, F[_]] {

  def transform[T]: FQueryTranform[F[(FAtomicValueImpl[T], U)]]

  val monad: Monad[F]
  val semigroup: Semigroup[U]
  val aa = cats.data.Validated

  val listTraverse: Traverse[List] = implicitly[Traverse[List]]

  def unzip[T](fab: F[(FAtomicValueImpl[T], U)]): (F[FAtomicValueImpl[T]], F[U]) =
    (monad.map(fab)(_._1), monad.map(fab)(_._2))

  def listTraverse[T](a: List[F[T]]): F[List[T]] = {
    listTraverse.sequence(a)(monad)
  }

}