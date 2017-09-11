package net.scalax.ubw.core

import scala.language.higherKinds
import scala.reflect.runtime.universe._
import shapeless._

trait QueryTranform[U] {
  type QueryType
  val path: AtomicPath
  val gen: Either[AtomicException, QueryType]
  def apply(rep: QueryType, data: AtomicValueImpl[path.DataType]): U
}
/*trait QueryTranformWithOutData[U] {
  type QueryType
  val gen: Either[AtomicException, QueryType]
  def apply(rep: QueryType): U
}*/
trait AtomicQueryImpl {
  self =>

  val path: AtomicPath

  val FANil: AbstractAtomicGen[path.DataType, HNil] = AbstractAtomicGen.empty

  def withRep[F](rep: AbstractAtomicGen[path.DataType, F]): WithRep[F] = {
    new WithRep[F] {
      override val queryResult: Either[AtomicException, F] = {
        rep.getBy(path.atomics)
      }
    }
  }

  trait WithRep[A] {
    val queryResult: Either[AtomicException, A]

    def mapTo[R](cv: (A, AtomicValueImpl[path.DataType]) => R): QueryTranform[R] = {
      new QueryTranform[R] {
        override type QueryType = A
        override lazy val path: self.path.type = self.path
        override val gen = queryResult
        override def apply(rep: A, data: AtomicValueImpl[path.DataType]): R = {
          cv(rep, data)
        }
      }
    }
    //def mapToOption[R](cv: (A, AtomicValueImpl[path.DataType]) => R): QueryTranform[R] = mapTo[R](cv)
    /*def mapToWithoutData[R](cv: A => R): QueryTranformWithOutData[R] = {
      new QueryTranformWithOutData[R] {
        override type QueryType = A
        override val gen = queryResult
        override def apply(rep: A): R = {
          cv(rep)
        }
      }
    }*/
    //def mapToOptionWithoutData[R](cv: A => R): QueryTranformWithOutData[R] = mapToWithoutData[R](cv)
  }

  def needAtomic[T[_]](implicit parGen: AtomicPartialFunctionGen[T], typeTag: WeakTypeTag[T[_]]): AbstractAtomicGen[path.DataType, T[path.DataType]] = AtomicGenHelper.needAtomic[path.DataType, T](parGen, typeTag)
  def needAtomicOpt[T[_]](implicit parGen: AtomicPartialFunctionGen[T]): AbstractAtomicGen[path.DataType, Option[T[path.DataType]]] = AtomicGenHelper.needAtomicOpt[path.DataType, T](parGen)
  def needAtomicList[T[_]](implicit parGen: AtomicPartialFunctionGen[T]): AbstractAtomicGen[path.DataType, List[T[path.DataType]]] = AtomicGenHelper.needAtomicList[path.DataType, T](parGen)

}

class AtomicQuery(override val path: AtomicPath) extends AtomicQueryImpl