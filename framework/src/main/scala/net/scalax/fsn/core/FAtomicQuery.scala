package net.scalax.fsn.core

import scala.language.higherKinds
import scala.language.existentials

trait AbstractFAtomicQuery[F[_]] {

  def gen[D](atomics: List[FAtomic[D]]): F[D]

}

trait FAtomicQuery[E, F[_]] extends AbstractFAtomicQuery[F] {

  val rep: E

  val atomicSape: FAtomicShape[E] { type U[K] = F[K] }

  override def gen[D](atomics: List[FAtomic[D]]): F[D] = {
    atomicSape.wrap(atomicSape.unwrap(rep).map { s => s.gen(atomics) })
  }

}

object FAtomicQuery {

  def apply[E](rep1: E)(implicit shape: FAtomicShape[E]): FAtomicQuery[E, shape.U] = new FAtomicQuery[E, shape.U] {
    override val rep = rep1
    override val atomicSape  = shape: FAtomicShape[E] { type U[K] = shape.U[K] }
  }

}

trait FAtomicShape[-E] {

  type U[_]

  val needWrapLength: Int
  def unwrap(rep: E): List[AbstractFAtomicGen]
  def wrap[D](atomics: List[Any]): U[D]

}

trait FAtomicShapeImpl {

  import shapeless._

  type FNil[_] = HNil

  implicit val hNilHListAtomicShape: FAtomicShape[HNil] { type U[_] = HNil } = {
    new FAtomicShape[HNil] {

      override type U[_] = HNil

      override val needWrapLength = 0
      override def unwrap(rep: HNil): List[AbstractFAtomicGen] = Nil
      override def wrap[D](atomics: List[Any]): HNil = HNil

    }
  }

  implicit def repLikeAtomicShape[S[_]]: FAtomicShape[FAtomicGen[S]] { type U[K] = S[K] } = {
    new FAtomicShape[FAtomicGen[S]] {

      override type U[K] = S[K]

      override val needWrapLength = 1
      override def unwrap(rep: FAtomicGen[S]): List[AbstractFAtomicGen] = rep :: Nil
      override def wrap[D](atomics: List[Any]): S[D] = atomics.head.asInstanceOf[S[D]]

    }
  }

  implicit def hListAtomicShape[S <: HList, E, A <: HList]
  (implicit repConvert: S <:< (E :: A), subShape: FAtomicShape[E], tailShape: FAtomicShape[A] { type U[K] <: HList })
  : FAtomicShape[S] { type U[K] = subShape.U[K] :: tailShape.U[K] } = {
    new FAtomicShape[S] {

      override type U[K] = subShape.U[K] :: tailShape.U[K]

      override val needWrapLength = subShape.needWrapLength + tailShape.needWrapLength

      override def unwrap(rep: S): List[AbstractFAtomicGen] = {
        val subRep :: tailRep = repConvert(rep)
        subShape.unwrap(subRep) ::: tailShape.unwrap(tailRep)
      }

      override def wrap[D](atomics: List[Any]): subShape.U[D] :: tailShape.U[D] = {
        (subShape.wrap(atomics.take(subShape.needWrapLength)): subShape.U[D]) :: (tailShape.wrap(atomics.drop(subShape.needWrapLength)): tailShape.U[D])
      }

    }
  }

}