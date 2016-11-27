package net.scalax.fsn.core

import scala.language.higherKinds
import scala.language.existentials
import shapeless._

trait AbstractFAtomicQuery[F[_]] {

  def gen[D](atomics: List[FAtomic[D]]): F[D]

  def map[U, X](path: FPath)(cv: F[path.DataType] => X): X = {
    cv(gen(path.atomics))
  }

}

/*trait FQueryTranform[U, X] {

  def apply(atomics: List[FAtomic[U]]): X

}*/

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

trait FAtomicGenOptHelper[E[_]] {
  type U[K] = Option[E[K]]
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

  implicit def repLikeAtomicOptShape[S[_]]: FAtomicShapeImpl[FAtomicGenOpt[S], FAtomicGenOptHelper[S]#U] = {
    new FAtomicShapeImpl[FAtomicGenOpt[S], FAtomicGenOptHelper[S]#U] {

      override val needWrapLength = 1
      override def unwrap(rep: FAtomicGenOpt[S]): List[AbstractFAtomicGen] = rep :: Nil
      override def wrap[D](atomics: List[Any]): Option[S[D]] = atomics.head.asInstanceOf[Option[S[D]]]

    }
  }

  implicit def hListAtomicShape[S <: HList, E, A <: HList]
  (implicit repConvert: S <:< (E :: A), subShape: FAtomicShape[E], tailShape: FAtomicShape.AuxHList[A])
  : FAtomicShapeImpl[S, FAtomicShapeTypeHelper[subShape.U, tailShape.U]#U] = {
    new FAtomicShapeImpl[S, FAtomicShapeTypeHelper[subShape.U, tailShape.U]#U] {

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
