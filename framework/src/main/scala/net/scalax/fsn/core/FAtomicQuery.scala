package net.scalax.fsn.core

import scala.language.higherKinds
import scala.language.existentials

trait FAtomicQuery[E, U[_]] {

  val rep: E

  val atomicSape: FAtomicShape[E, U]

  def gen[D](atomics: List[FAtomic[D]]): U[D] = {
    atomicSape.wrap(atomicSape.unwrap(rep).map { s => s.gen(atomics) })
  }

}

object FAtomicQuery {

  def apply[E, U[_]](rep1: E)(implicit shape: FAtomicShape[E, U]): FAtomicQuery[E, U] = new FAtomicQuery[E, U] {
    override val rep = rep1
    override val atomicSape = shape
  }

}

trait FAtomicShape[-E, U[_]] {

  def needWrapLength: Int
  def unwrap(rep: E): List[AbstractFAtomicGen]
  def wrap[D](atomics: List[Any]): U[D]

}

trait FAtomicShapeImpl {

  import shapeless._

  type FNil[_] = HNil

  implicit val hNilHListAtomicShape: FAtomicShape[HNil, FNil] = {
    new FAtomicShape[HNil, FNil] {

      override def needWrapLength = 0
      override def unwrap(rep: HNil): List[AbstractFAtomicGen] = Nil
      override def wrap[D](atomics: List[Any]): FNil[D] = HNil

    }
  }

  implicit def repLikeAtomicShape[S[_]]: FAtomicShape[FAtomicGen[S], S] = {
    new FAtomicShape[FAtomicGen[S], S] {

      override def needWrapLength = 1
      override def unwrap(rep: FAtomicGen[S]): List[AbstractFAtomicGen] = rep :: Nil
      override def wrap[D](atomics: List[Any]): S[D] = atomics.head.asInstanceOf[S[D]]

    }
  }

  /*implicit def hListAtomicShape[S, T[_], A <: HList, B[I] <: HList forSome { type I }]
  (implicit subShape: FAtomicShape[S, T], tailShape: FAtomicShape[A, B]): FAtomicShape[S :: A, (T[H] :: B[H])] forSome { type H } = {

    type E[K] = T[K] :: B[K]

    new FAtomicShape[S :: A, E] {

      override def needWrapLength = subShape.needWrapLength + tailShape.needWrapLength

      override def unwrap(rep: S :: A): List[AbstractFAtomicGen] = {
        val subRep :: tailRep = rep
        subShape.unwrap(subRep) ::: tailShape.unwrap(tailRep)
      }
      override def wrap[D](atomics: List[Any]): E[D] = {
        (subShape.wrap(atomics.take(subShape.needWrapLength)): T[D]) :: (tailShape.wrap(atomics.drop(tailShape.needWrapLength)): B[D])
      }
,  forSome { type H }
    }
  }*/

  /*implicit def hListAtomicShape[S <: HList, T[_], U, V[_], A <: HList, B[H] <: HList]
  (implicit repConvert: S <:< (U :: A), subShape: FAtomicShape[U, V], tailShape: FAtomicShape[A, B]): FAtomicShape[S, J] = {
    new FAtomicShape[S, J] {

      override def needWrapLength = subShape.needWrapLength + tailShape.needWrapLength

      override def unwrap(rep: S): List[AbstractFAtomicGen] = {
        val subRep :: tailRep = repConvert(rep)
        subShape.unwrap(subRep) ::: tailShape.unwrap(tailRep)
      }

      override def wrap[D](atomics: List[Any]): J[D] = {
        //val aa = atomicConvert.asInstanceOf[(V[D] :: B[D]) <:< T[D]]
        (((subShape.wrap(atomics.take(subShape.needWrapLength)): V[D]) :: ((tailShape.wrap(atomics.drop(tailShape.needWrapLength))): B[D])))
      }

    }
  }*/

}