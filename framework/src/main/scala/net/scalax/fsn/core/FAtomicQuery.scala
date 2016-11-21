package net.scalax.fsn.core

import scala.language.higherKinds

trait FAtomicQuery[E[_], U[_]] {

  val rep: E

  val atomicSape: FAtomicShape[E, U]

  def gen[D](atomics: List[FAtomic[D]]): U[D] = {
    atomicSape.wrap(atomicSape.unwrap(rep).map { s => s.gen(atomics) })
  }

}

object FAtomicQuery {

  def apply[E, D, U](rep1: E)(shape: FAtomicShape[E, D, U]): FAtomicQuery[E, D, U] = new FAtomicQuery[E, D, U] {
    override val rep = rep1
    override val atomicSape = shape
  }

}

trait FAtomicShape[E[_], U[_]] {

  def unwrap[D](rep: E[D]): List[AbstractFAtomicGen]
  def wrap[D](atomics: List[Any]): U[D]

}

object FAtomicShape {

  import shapeless._

  implicit val hNilHListAtomicShape: FAtomicShape[HNil, HNil] = {
    new FAtomicShape[HNil, HNil] {

      def unwrap(rep: HNil) = Nil
      def wrap(atomics: List[Any]) = HNil

    }
  }

  implicit def repLikeAtomicShape[S[_]]: FAtomicShape[FAtomicGen[S], S] = {
    new FAtomicShape[FAtomicGen[S], S] {

      def unwrap(rep: FAtomicGen[S]) = rep :: Nil
      def wrap(atomics: List[Any]) = atomics(0).asInstanceOf[S]

    }
  }

  implicit def hListAtomicShape[S <: HList, T <: HList, U, V, A <: HList, B <: HList]
  (implicit repConvert: S <:< (U :: A), atomicConvert: T <:< (V :: B), subShape: FAtomicShape[U, V], tailShape: FAtomicShape[A, B]): FAtomicShape[S, T] = {
    new FAtomicShape[S, T] {

      def unwrap(rep: S) = {
        val subRep :: tailRep :: HNil = repConvert(rep)
        subShape.unwrap(subRep) ::: tailShape.unwrap(tailRep)
      }
      def wrap(atomics: List[Any]) = { ??? }

    }
  }

}