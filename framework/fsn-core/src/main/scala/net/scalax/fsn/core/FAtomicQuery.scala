package net.scalax.fsn.core

import scala.language.higherKinds
import shapeless._
import shapeless.ops.hlist.IsHCons

import scala.reflect.runtime.universe._

trait FAtomicQueryImpl {
  self =>

  val path: FPath

  def withRep[U, S, Y](rep: U)(implicit fAtomicGenShape: FAtomicGenShape[U, path.DataType, Y]): Abc[Y] = {
    new Abc[Y] {
      override val queryResult = fAtomicGenShape.unwrap(rep).getBy(path.atomics)
    }
  }

  /*override def mapToOption[R](cv: (T, Option[path.DataType]) => R): FQueryTranform[T, Option, R]

    override def mapToWithoutData[R](cv: T => R): FQueryTranformWithOutData[R]

    override def mapToOptionWithoutData[R](cv: T => R): FQueryTranformWithOutData[R]
    }*/

  trait Abc[A] {
    val queryResult: Either[FAtomicException, A]

    def mapTo[C[_], R](cv: (A, C[path.DataType]) => R): FQueryTranform[R, C] = {
      new FQueryTranform[R, C] {
        override type QueryType = A
        override lazy val path: self.path.type = self.path
        override val gen = queryResult
        override def apply(rep: A, data: C[path.DataType]): R = {
          //t.right.map(s => cv(s, data))
          cv(rep, data)
        }
      }
    }

    def mapToOption[R](cv: (A, Option[path.DataType]) => R): FQueryTranform[R, Option] = mapTo[Option, R](cv)

    def mapToWithoutData[C[_], R](cv: A => R): FQueryTranformWithOutData[R, C] = {
      new FQueryTranformWithOutData[R, C] {
        override type QueryType = A
        override val gen = queryResult
        override def apply(rep: A): R = {
          cv(rep)
        }
      }
    }

    def mapToOptionWithoutData[R](cv: A => R): FQueryTranformWithOutData[R, Option] = mapToWithoutData[Option, R](cv)
  }

  def needAtomic[T[_]](implicit parGen: FAtomicPartialFunctionGen[T], typeTag: WeakTypeTag[T[_]]): FAtomicGen[path.DataType, T] = FAtomicGenHelper.needAtomic[path.DataType, T](parGen, typeTag)
  def needAtomicOpt[T[_]](implicit parGen: FAtomicPartialFunctionGen[T]): FAtomicGenOpt[path.DataType, T] = FAtomicGenHelper.needAtomicOpt[path.DataType, T](parGen)
  def needAtomicList[T[_]](implicit parGen: FAtomicPartialFunctionGen[T]): FAtomicGenList[path.DataType, T] = FAtomicGenHelper.needAtomicList[path.DataType, T](parGen)

}

class FAtomicQuery(override val path: FPath) extends FAtomicQueryImpl

/*object FAtomicQuery {
  def apply(path: FPath): FAtomicQuery = {
    new FAtomicQueryImpl(path)
  }
}*/

trait FQueryTranform[U, C[_]] {
  type QueryType
  val path: FPath
  val gen: Either[FAtomicException, QueryType]
  def apply(rep: QueryType, data: C[path.DataType]): U

  /*def withList[Y](columnGen: List[U] => Y): FPileSyntax.PileGen[C, Y] = {
    FPile.transformTreeList(this)(columnGen)
  }*/
}

trait FQueryTranformWithOutData[U, C[_]] {
  type QueryType
  val gen: Either[FAtomicException, QueryType]
  def apply(rep: QueryType): U

  /*def withList[Y](columnGen: List[U] => Y): FPileSyntaxWithoutData.PileGen[C, Y] = {
    FPile.transformTreeListWithoutData(this)(columnGen)
  }*/
}

/*trait AbstractFAtomicQuery[F[_]] { self =>

  def gen[D](atomics: List[FAtomic[D]]): Either[FAtomicException, F[D]]

  def mapTo[T, C[_]](path: FPath)(cv: (F[path.DataType], C[path.DataType]) => T): FQueryTranform[T, C] = {
    new FQueryTranformImpl[F, C, T] {
      override val fPath: path.type = path
      override lazy val gen: Either[FAtomicException, QueryType[fPath.DataType]] = {
        self.gen(path.atomics)
      }
      def apply(atomics: QueryType[fPath.DataType], data: C[fPath.DataType]): T = {
        cv(atomics, data)
      }
    }
  }

  def mapToOption[T](path: FPath)(cv: (F[path.DataType], Option[path.DataType]) => T): FQueryTranform[T, Option] = {
    mapTo(path)(cv)
  }

  def mapToWithoutData[T, C[_]](path: FPath)(cv: F[path.DataType] => T): FQueryTranformWithOutData[T, C] = {
    new FQueryTranformWithOutDataImpl[F, C, T] {
      override val fPath: path.type = path
      override lazy val gen: Either[FAtomicException, QueryType[fPath.DataType]] = {
        self.gen(path.atomics)
      }
      def apply(atomics: QueryType[fPath.DataType]): T = {
        cv(atomics)
      }
    }
  }

  def mapToOptionWithoutData[T](path: FPath)(cv: F[path.DataType] => T): FQueryTranformWithOutData[T, Option] = {
    mapToWithoutData(path)(cv)
  }

}

trait FQueryTranform[T, C[_]] {

  type QueryType[_]

  val fPath: FPath
  val gen: Either[FAtomicException, QueryType[fPath.DataType]]
  def apply(atomics: QueryType[fPath.DataType], data: C[fPath.DataType]): T
}

trait FQueryTranformImpl[F[_], G[_], T] extends FQueryTranform[T, G] {

  override type QueryType[A] = F[A]

}

trait FQueryTranformWithOutData[T, C[_]] {

  type QueryType[_]

  val fPath: FPath
  val gen: Either[FAtomicException, QueryType[fPath.DataType]]
  def apply(atomics: QueryType[fPath.DataType]): T
}

trait FQueryTranformWithOutDataImpl[F[_], G[_], T] extends FQueryTranformWithOutData[T, G] {

  override type QueryType[A] = F[A]

}

trait FAtomicQuery[E, F[_]] extends AbstractFAtomicQuery[F] {

  val rep: E

  val atomicSape: FAtomicShape[E, F]

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

  def apply[E, F[_]](rep1: E)(implicit shape: FAtomicShape[E, F]): FAtomicQuery[E, F] = new FAtomicQuery[E, F] {
    override val rep = rep1
    override val atomicSape = shape: FAtomicShape[E, F]
  }

}

trait FAtomicShape[E, F[_]] {

  val needWrapLength: Int
  def unwrap(rep: E): List[AbstractFAtomicGen]
  def wrap[D](atomics: Either[FAtomicException, List[Any]]): Either[FAtomicException, F[D]]

}

object FAtomicShape {
  //type Aux[E, F[_]] = FAtomicShape[E] { type U[K] = F[K] }
  //type AuxHList[E] = FAtomicShape[E] { type U[K] <: HList }
}

/*trait FAtomicShapeImpl[E, F[_]] extends FAtomicShape[E] {

  override type U[K] = F[K]

  val needWrapLength: Int
  def unwrap(rep: E): List[AbstractFAtomicGen]
  def wrap[D](atomics: Either[FAtomicException, List[Any]]): Either[FAtomicException, U[D]]

}*/

trait FAtomicShapeTypeHelper[E[_], F[_] <: HList] {
  type U[K] = E[K] :: F[K]
}

trait FAtomicGenOptHelper[E[_]] {
  type U[K] = Option[E[K]]
}

trait FAtomicShapeHelper {

  type FNil[_] = HNil

  implicit final val hNilHListAtomicShape: FAtomicShape[HNil, FNil] = {
    new FAtomicShape[HNil, FNil] {

      override val needWrapLength = 0
      override def unwrap(rep: HNil): List[AbstractFAtomicGen] = Nil
      override def wrap[D](atomics: Either[FAtomicException, List[Any]]): Either[FAtomicException, FNil[D]] = Right(HNil)

    }
  }

  implicit final def repLikeAtomicShape[S[_]]: FAtomicShape[FAtomicGen[S], S] = {
    new FAtomicShape[FAtomicGen[S], S] {

      override val needWrapLength = 1
      override def unwrap(rep: FAtomicGen[S]): List[AbstractFAtomicGen] = rep :: Nil
      override def wrap[D](atomics: Either[FAtomicException, List[Any]]): Either[FAtomicException, S[D]] = atomics.right.map(_.head.asInstanceOf[S[D]])

    }
  }

  implicit final def repLikeAtomicOptShape[S[_]]: FAtomicShape[FAtomicGenOpt[S], FAtomicGenOptHelper[S]#U] = {
    new FAtomicShape[FAtomicGenOpt[S], FAtomicGenOptHelper[S]#U] {

      override val needWrapLength = 1
      override def unwrap(rep: FAtomicGenOpt[S]): List[AbstractFAtomicGen] = rep :: Nil
      override def wrap[D](atomics: Either[FAtomicException, List[Any]]): Either[FAtomicException, Option[S[D]]] = atomics.right.map(_.head.asInstanceOf[Option[S[D]]])

    }
  }

  implicit final def hListAtomicShape[ /*S <: HList,*/ E, F[_], A <: HList, B[_] <: HList](
    implicit
    //repConvert: IsHCons.Aux[S, E, A],
    subShape: FAtomicShape[E, F],
    tailShape: FAtomicShape[A, B] //{ type U[K] <: HList } //FAtomicShape.AuxHList[A]
  ): FAtomicShape[E :: A, FAtomicShapeTypeHelper[F, B]#U] = {
    new FAtomicShape[E :: A, FAtomicShapeTypeHelper[F, B]#U] {

      override val needWrapLength = subShape.needWrapLength + tailShape.needWrapLength

      override def unwrap(rep: E :: A): List[AbstractFAtomicGen] = {
        val subRep :: tailRep = rep
        //val subRep = repConvert.head(rep)
        //val tailRep = repConvert.tail(rep)
        subShape.unwrap(subRep) ::: tailShape.unwrap(tailRep)
      }

      override def wrap[D](atomics: Either[FAtomicException, List[Any]]): Either[FAtomicException, F[D] :: B[D]] = {
        ((subShape.wrap(atomics.right.map(_.take(subShape.needWrapLength))): Either[FAtomicException, F[D]]) -> (tailShape.wrap(atomics.right.map(_.drop(subShape.needWrapLength))): Either[FAtomicException, B[D]])) match {
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

object Aaa extends FAtomicGenHelper with FAtomicShapeHelper {
  trait Bbb[T] extends FAtomic[T] {
    val ccc: String = "1234"
  }
  FAtomicQuery(needAtomic[Bbb] :: HNil)(hListAtomicShape(repLikeAtomicShape, hNilHListAtomicShape))
  hListAtomicShape(repLikeAtomicShape[Bbb], hNilHListAtomicShape)
}*/ 