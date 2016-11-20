package net.scalax.fsn.core

import scala.language.higherKinds

/*trait FAtomicQuery[E, U] {

  type RepType = E
  type AtomicType = U

  val rep: E

  val atomicSape: FAtomicShape[E, U]

  def gen(path: FPath): U = {
    atomicSape.wrap(atomicSape.unwrap(rep).map { s => s.gen(path) })
  }

}

object FAtomicQuery {

  def apply[E, U](rep1: E)(shape: FAtomicShape[E, U]): FAtomicQuery[E, U] = new FAtomicQuery[E, U] {
    override val rep = rep1
    override val atomicSape = shape
  }

}

trait FAtomicShape[E, U] {

  type RepType = E
  type AtomicType = U

  def unwrap(rep: E): List[AbstractFAtomicGen]
  def wrap(atomics: List[Any]): U

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

}*/