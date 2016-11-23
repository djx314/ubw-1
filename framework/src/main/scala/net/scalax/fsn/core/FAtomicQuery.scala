package net.scalax.fsn.core

import scala.language.higherKinds
import scala.language.existentials
import shapeless._

trait AbstractFAtomicQuery[F[_]] {

  def gen[D](atomics: List[FAtomic[D]]): F[D]

}

trait FAtomicQuery[E, F[_]] extends AbstractFAtomicQuery[F] {

  val rep: E

  val atomicSape: FAtomicShape.Aux[E, F]

  override def gen[D](atomics: List[FAtomic[D]]): F[D] = {
    atomicSape.wrap(atomicSape.unwrap(rep).map { s => s.gen(atomics) })
  }

}

object FAtomicQuery {

  def apply[E](rep1: E)(implicit shape: FAtomicShape[E]): FAtomicQuery[E, shape.U] = new FAtomicQuery[E, shape.U] {
    override val rep = rep1
    override val atomicSape  = shape: FAtomicShape.Aux[E, shape.U]
  }

}

trait FAtomicShape[-E] {

  type U[_]

  val needWrapLength: Int
  def unwrap(rep: E): List[AbstractFAtomicGen]
  def wrap[D](atomics: List[Any]): U[D]

}

object FAtomicShape {
  type Aux[E, F[_]] = FAtomicShape[E] { type U[K] = F[K] }
  type AuxHList[E] = FAtomicShape[E] { type U[K] <: HList }
}

trait FAtomicShapeImpl[-E, F[_]] extends FAtomicShape[E] {

  type U[K] = F[K]

  val needWrapLength: Int
  def unwrap(rep: E): List[AbstractFAtomicGen]
  def wrap[D](atomics: List[Any]): U[D]

}

trait FAtomicShapeTypeHelper[E[_], F[_] <: HList] {
  type U[K] = E[K] :: F[K]
}

trait FAtomicShapeHelper {

  type FNil[_] = HNil

  implicit val hNilHListAtomicShape: FAtomicShapeImpl[HNil, FNil] = {
    new FAtomicShapeImpl[HNil, FNil] {

      override val needWrapLength = 0
      override def unwrap(rep: HNil): List[AbstractFAtomicGen] = Nil
      override def wrap[D](atomics: List[Any]): HNil = HNil

    }
  }

  implicit def repLikeAtomicShape[S[_]]: FAtomicShapeImpl[FAtomicGen[S], S] = {
    new FAtomicShapeImpl[FAtomicGen[S], S] {

      override val needWrapLength = 1
      override def unwrap(rep: FAtomicGen[S]): List[AbstractFAtomicGen] = rep :: Nil
      override def wrap[D](atomics: List[Any]): S[D] = atomics.head.asInstanceOf[S[D]]

    }
  }

  implicit def hListAtomicShape[S <: HList, E, A <: HList, F[_]]
  (implicit repConvert: S <:< (E :: A), subShape: FAtomicShapeImpl[E, F], tailShape: FAtomicShape.AuxHList[A])
  : FAtomicShapeImpl[S, FAtomicShapeTypeHelper[F, tailShape.U]#U] = {
    new FAtomicShapeImpl[S, FAtomicShapeTypeHelper[F, tailShape.U]#U] {

      override val needWrapLength = subShape.needWrapLength + tailShape.needWrapLength

      override def unwrap(rep: S): List[AbstractFAtomicGen] = {
        val subRep :: tailRep = repConvert(rep)
        subShape.unwrap(subRep) ::: (tailShape).unwrap(tailRep)
      }

      override def wrap[D](atomics: List[Any]): F[D] :: tailShape.U[D] = {
        (subShape.wrap(atomics.take(subShape.needWrapLength)): F[D]) :: (tailShape.wrap(atomics.drop(subShape.needWrapLength)): tailShape.U[D])
      }

    }
  }

}