package net.scalax.ubw.core

import cats.{ Functor, Monad, Semigroup, Traverse }
import cats.instances.list._
import cats._

import net.scalax.fsn.core._

import scala.language.higherKinds

trait PileFilter[U, F[_]] {

  val transform: FAtomicPath => FQueryTranform[F[(FAtomicValue, U)]]

  val monad: Monad[F]
  val semigroup: Semigroup[U]

  val mixSemigroup: Semigroup[F[U]] = {
    new Semigroup[F[U]] {
      override def combine(x: F[U], y: F[U]): F[U] = {
        monad.flatMap(x) { a => monad.map(y) { b => semigroup.combine(a, b) } }
      }
    }
  }

  val listTraverse: Traverse[List] = implicitly[Traverse[List]]

  def unzip[T](fab: F[(FAtomicValue, U)]): (F[FAtomicValue], F[U]) =
    (monad.map(fab)(_._1), monad.map(fab)(_._2))

  def listTraverse[T](a: List[F[T]]): F[List[T]] = {
    listTraverse.sequence(a)(monad)
  }

}

object PileFilter {

  def empty: PileFilter[shapeless.HNil, cats.Id] = new PileFilter[shapeless.HNil, cats.Id] {
    override val transform = { fAtomicPath =>
      new FQueryTranform[cats.Id[(FAtomicValue, shapeless.HNil)]] {
        type QueryType = shapeless.HList
        val path = fAtomicPath
        val gen: Either[FAtomicException, QueryType] = Right(shapeless.HNil)
        def apply(rep: QueryType, data: FAtomicValueImpl[path.DataType]): cats.Id[(FAtomicValue, shapeless.HNil)] = data -> shapeless.HNil
      }
    }
    override val monad = implicitly[Monad[cats.Id]]
    override val semigroup = new Semigroup[shapeless.HNil] {
      override def combine(x: shapeless.HNil, y: shapeless.HNil): shapeless.HNil = {
        shapeless.HNil
      }
    }
  }

  def apply[U, F[_]](gen: FAtomicPath => FQueryTranform[F[(FAtomicValue, U)]])(implicit monad1: Monad[F], semigroup1: Semigroup[U]): PileFilter[U, F] = {
    new PileFilter[U, F] {
      override val transform = gen
      override val monad = monad1
      override val semigroup = semigroup1
    }
  }

}