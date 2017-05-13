package net.scalax.fsn.core

import scala.language.higherKinds
import scala.reflect.runtime.universe._
import shapeless._

trait FQueryTranform[U] {
  type QueryType
  val path: FAtomicPath
  val gen: Either[FAtomicException, QueryType]
  def apply(rep: QueryType, data: FAtomicValueImpl[path.DataType]): U
}

trait FQueryTranformWithOutData[U] {
  type QueryType
  val gen: Either[FAtomicException, QueryType]
  def apply(rep: QueryType): U
}

trait FAtomicQueryImpl {
  self =>

  val path: FAtomicPath

  val FANil: AbstractFAtomicGen[path.DataType, HNil] = AbstractFAtomicGen.empty

  def withRep[F](rep: AbstractFAtomicGen[path.DataType, F]): WithRep[F] = {
    new WithRep[F] {
      override val queryResult: Either[FAtomicException, F] = {
        rep.getBy(path.atomics)
      }
    }
  }

  trait WithRep[A] {
    val queryResult: Either[FAtomicException, A]

    def mapTo[R](cv: (A, FAtomicValueImpl[path.DataType]) => R): FQueryTranform[R] = {
      new FQueryTranform[R] {
        override type QueryType = A
        override lazy val path: self.path.type = self.path
        override val gen = queryResult
        override def apply(rep: A, data: FAtomicValueImpl[path.DataType]): R = {
          cv(rep, data)
        }
      }
    }

    //def mapToOption[R](cv: (A, FAtomicValueImpl[path.DataType]) => R): FQueryTranform[R] = mapTo[R](cv)

    def mapToWithoutData[R](cv: A => R): FQueryTranformWithOutData[R] = {
      new FQueryTranformWithOutData[R] {
        override type QueryType = A
        override val gen = queryResult
        override def apply(rep: A): R = {
          cv(rep)
        }
      }
    }

    //def mapToOptionWithoutData[R](cv: A => R): FQueryTranformWithOutData[R] = mapToWithoutData[R](cv)
  }

  def needAtomic[T[_]](implicit parGen: FAtomicPartialFunctionGen[T], typeTag: WeakTypeTag[T[_]]): AbstractFAtomicGen[path.DataType, T[path.DataType]] = FAtomicGenHelper.needAtomic[path.DataType, T](parGen, typeTag)
  def needAtomicOpt[T[_]](implicit parGen: FAtomicPartialFunctionGen[T]): AbstractFAtomicGen[path.DataType, Option[T[path.DataType]]] = FAtomicGenHelper.needAtomicOpt[path.DataType, T](parGen)
  def needAtomicList[T[_]](implicit parGen: FAtomicPartialFunctionGen[T]): AbstractFAtomicGen[path.DataType, List[T[path.DataType]]] = FAtomicGenHelper.needAtomicList[path.DataType, T](parGen)

}

class FAtomicQuery(override val path: FAtomicPath) extends FAtomicQueryImpl