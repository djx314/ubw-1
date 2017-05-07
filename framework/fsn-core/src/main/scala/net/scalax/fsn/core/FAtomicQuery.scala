package net.scalax.fsn.core

import scala.language.higherKinds
import scala.reflect.runtime.universe._
import shapeless._

trait FAtomicQueryBase {
  self =>

  val path: FAtomicPath

  def commonWithRep[E, V, X, F, K](rep: E)(everywherePoly: Poly, everythingPoly: EverythingAux[F, K])(
    implicit
    case1: Lazy[poly.Case1.Aux[everywherePoly.type, E, V]],
    listCase: Lazy[poly.Case1.Aux[everythingPoly.type, E, X]],
    cv: Lazy[X <:< List[AbstractFAtomicGen[path.DataType, Any]]]
  ): WithRep[V] = {
    new WithRep[V] {
      override val queryResult = {
        try {
          Right(everywherePoly.apply(rep)(case1.value))
        } catch {
          case e: FAtomicException =>
            //过滤出全部 FAtomic 中没有的 Reader 的 TypeTag
            val atomicGenList = cv.value(
              everythingPoly
                .apply(rep)(listCase.value)
            )
            val missTypeTags = atomicGenList.map { gen =>
              gen.getBy(path.atomics) match {
                case Left(e) => Option(e)
                case _ => None
              }
            }.collect { case Some(s) => s.typeTags }.flatten
            Left(FAtomicException(missTypeTags))
        }
      }
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

}

trait FAtomicQueryImpl extends FAtomicQueryBase {
  self =>

  object fAtomicGenPolyImpl extends FAtomicGenPolyImpl[path.type](path)
  val everywhereFAtomicGenPoly = everywhere(fAtomicGenPolyImpl)

  object everythingFatomicListPoly extends FAtomicListPolyImpl[path.type](path)
  object everythingFAtomicAppend extends FAtomicAppendImpl[path.type](path)
  val everythingImpl = everything(everythingFatomicListPoly).apply(everythingFAtomicAppend)

  def withRep[E, V, X](rep: E)(
    implicit
    case1: Lazy[poly.Case1.Aux[everywhereFAtomicGenPoly.type, E, V]],
    listCase: Lazy[poly.Case1.Aux[everythingImpl.type, E, X]],
    cv: Lazy[X <:< List[AbstractFAtomicGen[path.DataType, Any]]]
  ): WithRep[V] = {
    commonWithRep(rep)(everywhereFAtomicGenPoly, everythingImpl)(case1, listCase, cv)
  }

  def needAtomic[T[_]](implicit parGen: FAtomicPartialFunctionGen[T], typeTag: WeakTypeTag[T[_]]): AbstractFAtomicGen[path.DataType, T[path.DataType]] = FAtomicGenHelper.needAtomic[path.DataType, T](parGen, typeTag)
  def needAtomicOpt[T[_]](implicit parGen: FAtomicPartialFunctionGen[T]): AbstractFAtomicGen[path.DataType, Option[T[path.DataType]]] = FAtomicGenHelper.needAtomicOpt[path.DataType, T](parGen)
  def needAtomicList[T[_]](implicit parGen: FAtomicPartialFunctionGen[T]): AbstractFAtomicGen[path.DataType, List[T[path.DataType]]] = FAtomicGenHelper.needAtomicList[path.DataType, T](parGen)

}

trait FAtomicGenPoly extends Poly1 {

  val path: FAtomicPath

  implicit def intCase[S]: Case.Aux[AbstractFAtomicGen[path.DataType, S], S] = {
    at { s =>
      s.getBy(path.atomics) match {
        case Left(e) => throw e
        case Right(t) => t
      }
    }
  }

}

class FAtomicGenPolyImpl[T <: FAtomicPath](override val path: T) extends FAtomicGenPoly

trait FAtomicListPoly extends Poly1 {

  val path: FAtomicPath

  implicit def default[T] = at[T](_ => List.empty[AbstractFAtomicGen[path.DataType, Any]])

  implicit def intCase[S]: Case.Aux[AbstractFAtomicGen[path.DataType, S], List[AbstractFAtomicGen[path.DataType, Any]]] = {
    at(s => s.asInstanceOf[AbstractFAtomicGen[path.DataType, Any]] :: Nil)
  }

}

class FAtomicListPolyImpl[T <: FAtomicPath](override val path: T) extends FAtomicListPoly

trait FAtomicAppend extends Poly2 {
  val path: FAtomicPath

  implicit def caseString = at[List[AbstractFAtomicGen[path.DataType, Any]], List[AbstractFAtomicGen[path.DataType, Any]]](_ ++ _)
}

class FAtomicAppendImpl[T <: FAtomicPath](override val path: T) extends FAtomicAppend

class FAtomicQuery(override val path: FAtomicPath) extends FAtomicQueryImpl

trait FQueryTranform[U, C[_]] {
  type QueryType
  val path: FAtomicPath
  val gen: Either[FAtomicException, QueryType]
  def apply(rep: QueryType, data: C[path.DataType]): U
}

trait FQueryTranformWithOutData[U, C[_]] {
  type QueryType
  val gen: Either[FAtomicException, QueryType]
  def apply(rep: QueryType): U
}