package net.scalax.fsn.core

import scala.language.higherKinds

import scala.reflect.runtime.universe._

import shapeless._

trait FAtomicQueryImpl {
  self =>

  val path: FPath

  object fAtomicGenPolyImpl extends FAtomicGenPolyImpl[path.type](path)

  val everyFAtomicGenPoly = everywhere(fAtomicGenPolyImpl)

  def withRep[E, V](rep: E)(
    implicit
    case1: poly.Case1.Aux[everyFAtomicGenPoly.type, E, V]
  ): WithRep[V] = {
    new WithRep[V] {
      override val queryResult = {
        try {
          Right(everyFAtomicGenPoly.apply(rep)(case1))
        } catch {
          case e: FAtomicException =>
            //TODO 这里还没有完成，要判断是缺少的所有 FAtomic
            Left(e)
        }
      } //Right(everyFAtomicGenPoly.apply(rep)(case1)) //fAtomicGenShape.unwrap(rep).getBy(path.atomics)
    }
  }

  trait WithRep[A] {
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

  def needAtomic[T[_]](implicit parGen: FAtomicPartialFunctionGen[T], typeTag: WeakTypeTag[T[_]]): AbstractFAtomicGen[path.DataType, T[path.DataType]] = FAtomicGenHelper.needAtomic[path.DataType, T](parGen, typeTag)
  def needAtomicOpt[T[_]](implicit parGen: FAtomicPartialFunctionGen[T]): AbstractFAtomicGen[path.DataType, Option[T[path.DataType]]] = FAtomicGenHelper.needAtomicOpt[path.DataType, T](parGen)
  def needAtomicList[T[_]](implicit parGen: FAtomicPartialFunctionGen[T]): AbstractFAtomicGen[path.DataType, List[T[path.DataType]]] = FAtomicGenHelper.needAtomicList[path.DataType, T](parGen)

}

trait FAtomicGenPoly extends Poly1 {

  val path: FPath

  implicit def intCase[S]: Case.Aux[AbstractFAtomicGen[path.DataType, S], S] = {
    at(s => s.getBy(path.atomics).right.get)
  }
}

class FAtomicGenPolyImpl[T <: FPath](override val path: T) extends FAtomicGenPoly

class FAtomicQuery(override val path: FPath) extends FAtomicQueryImpl

trait FQueryTranform[U, C[_]] {
  type QueryType
  val path: FPath
  val gen: Either[FAtomicException, QueryType]
  def apply(rep: QueryType, data: C[path.DataType]): U
}

trait FQueryTranformWithOutData[U, C[_]] {
  type QueryType
  val gen: Either[FAtomicException, QueryType]
  def apply(rep: QueryType): U
}