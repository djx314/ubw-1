package net.scalax.fsn.core

import scala.language.higherKinds
import scala.language.existentials
import shapeless._

trait AbstractFAtomicQuery[F[_]] { self =>

  def gen[D](atomics: List[FAtomic[D]]): Either[FAtomicException, F[D]]

  def map[U, X](path: FPath)(cv: F[path.DataType] => X): Either[FAtomicException, X] = {
    gen(path.atomics).right.map(cv)
  }

  def mapToOption[T](path: FPath)(cv: (F[path.DataType], Option[path.DataType]) => T): FQueryTranform[T] = {
    new FQueryTranformImpl[F, Option, T] {
      override val fPath: path.type = path
      override lazy val gen: Either[FAtomicException, QueryType[fPath.DataType]] = {
        self.gen(path.atomics)
      }
      def apply(atomics: QueryType[fPath.DataType], data: WrapType[fPath.DataType]): T = {
        cv(atomics, data)
      }
    }
  }

}

trait FQueryTranform[T] {

  type QueryType[_]
  type WrapType[_]

  val fPath: FPath
  val gen: Either[FAtomicException, QueryType[fPath.DataType]]
  def apply(atomics: QueryType[fPath.DataType], data: WrapType[fPath.DataType]): T
  /*def output(data: WrapType[fPath.DataType]): Either[FAtomicException, T] = {
    gen.right.map(s => apply(s, data))
  }*/
}

trait FQueryTranformImpl[F[_], G[_], T] extends FQueryTranform[T] {

  override type QueryType[A] = F[A]
  override type WrapType[A] = G[A]

}

trait FAtomicQuery[E, F[_]] extends AbstractFAtomicQuery[F] {

  val rep: E

  val atomicSape: FAtomicShape.Aux[E, F]

  override def gen[D](atomics: List[FAtomic[D]]): Either[FAtomicException, F[D]] = {
    atomicSape.wrap(atomicSape.unwrap(rep).map { s => s.getBy(atomics) }.foldLeft(Right(Nil): Either[FAtomicException, List[Any]]) { (font, end) =>
      (font -> end) match {
        case (Left(s), Left(t)) =>
          Left(FAtomicException(s.typeTags ::: t.typeTags))
        case (Left(s), Right(_)) =>
          Left(FAtomicException(s.typeTags))
        case (Right(_), Left(s)) =>
          Left(FAtomicException(s.typeTags))
        case (Right(s), Right(t)) =>
          Right(s ::: t :: Nil)
      }
    })
  }

}

object FAtomicQuery {

  def apply[E](rep1: E)(implicit shape: FAtomicShape[E]): FAtomicQuery[E, shape.U] = new FAtomicQuery[E, shape.U] {
    override val rep = rep1
    override val atomicSape = shape: FAtomicShape.Aux[E, shape.U]
  }

}

trait FAtomicShape[-E] {

  type U[_]

  val needWrapLength: Int
  def unwrap(rep: E): List[AbstractFAtomicGen]
  def wrap[D](atomics: Either[FAtomicException, List[Any]]): Either[FAtomicException, U[D]]

}

object FAtomicShape {
  type Aux[E, F[_]] = FAtomicShape[E] { type U[K] = F[K] }
  type AuxHList[E] = FAtomicShape[E] { type U[K] <: HList }
}

trait FAtomicShapeImpl[-E, F[_]] extends FAtomicShape[E] {

  type U[K] = F[K]

  val needWrapLength: Int
  def unwrap(rep: E): List[AbstractFAtomicGen]
  def wrap[D](atomics: Either[FAtomicException, List[Any]]): Either[FAtomicException, U[D]]

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
      override def wrap[D](atomics: Either[FAtomicException, List[Any]]): Either[FAtomicException, FNil[D]] = Right(HNil)

    }
  }

  implicit def repLikeAtomicShape[S[_]]: FAtomicShapeImpl[FAtomicGen[S], S] = {
    new FAtomicShapeImpl[FAtomicGen[S], S] {

      override val needWrapLength = 1
      override def unwrap(rep: FAtomicGen[S]): List[AbstractFAtomicGen] = rep :: Nil
      override def wrap[D](atomics: Either[FAtomicException, List[Any]]): Either[FAtomicException, S[D]] = atomics.right.map(_.head.asInstanceOf[S[D]])

    }
  }

  implicit def repLikeAtomicOptShape[S[_]]: FAtomicShapeImpl[FAtomicGenOpt[S], FAtomicGenOptHelper[S]#U] = {
    new FAtomicShapeImpl[FAtomicGenOpt[S], FAtomicGenOptHelper[S]#U] {

      override val needWrapLength = 1
      override def unwrap(rep: FAtomicGenOpt[S]): List[AbstractFAtomicGen] = rep :: Nil
      override def wrap[D](atomics: Either[FAtomicException, List[Any]]): Either[FAtomicException, Option[S[D]]] = atomics.right.map(_.head.asInstanceOf[Option[S[D]]])

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

      override def wrap[D](atomics: Either[FAtomicException, List[Any]]): Either[FAtomicException, subShape.U[D] :: tailShape.U[D]] = {
        ((subShape.wrap(atomics.right.map(_.take(subShape.needWrapLength))): Either[FAtomicException, subShape.U[D]]) -> (tailShape.wrap(atomics.right.map(_.drop(subShape.needWrapLength))): Either[FAtomicException, tailShape.U[D]])) match {
          case (Left(s), Left(t)) =>
            Left(FAtomicException(s.typeTags ::: t.typeTags))
          case (Left(s), Right(_)) =>
            Left(FAtomicException(s.typeTags))
          case (Right(_), Left(s)) =>
            Left(FAtomicException(s.typeTags))
          case (Right(s), Right(t)) =>
            Right(s :: t)
        }
      }

    }
  }

}
