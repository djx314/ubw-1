package net.scalax.fsn.mix.slickbase

import net.scalax.fsn.core.FPile
import net.scalax.fsn.slick.helpers.SlickQueryBindImpl
import slick.ast.{AnonSymbol, Ref}
import slick.lifted._

import scala.concurrent.ExecutionContext

/*class ListQueryExtensionMethods[E, U](val queryToExt: Query[E, U, Seq]) {

  def flatMap[A, B](f: E => ListQueryWrap): ListQueryWrap = {
    val generator = new AnonSymbol
    val aliased = queryToExt.shaped.encodeRef(Ref(generator)).value
    val fv = f(aliased)
    val slickJsonQuery = new SlickQueryBindImpl {
      override def bind[E, U](query: Query[E, U, Seq]): Query[E, U, Seq] = {
        val newQuery = fv.listQueryBind.bind(query)
        new WrappingQuery[E, U, Seq](new slick.ast.Bind(generator, queryToExt.toNode, newQuery.toNode), newQuery.shaped)
      }
    }
    ListQueryWrap(fv.columns, slickJsonQuery)(fv.ec)
  }

  def map(ev: E => List[FColumn])(implicit ec: ExecutionContext): ListQueryWrap = {
    val generator = new AnonSymbol
    val aliased = queryToExt.shaped.encodeRef(Ref(generator)).value
    val columns = ev(aliased)
    val slickJsonQuery = new SlickQueryBindImpl {
      override def bind[E, U](query: Query[E, U, Seq]): Query[E, U, Seq] = {
        new WrappingQuery[E, U, Seq](new slick.ast.Bind(generator, queryToExt.toNode, query.toNode), query.shaped)
      }
    }
    ListQueryWrap(columns, slickJsonQuery)
  }

  def filter[T <: Rep[_] : CanBeQueryCondition](f: E => T): ListQueryExtensionMethods[E, U] = {
    val cv = implicitly[CanBeQueryCondition[T]]
    new ListQueryExtensionMethods(queryToExt.filter(f)(cv))
  }

  def withFilter[T : CanBeQueryCondition](f: E => T): ListQueryExtensionMethods[E, U] = {
    val cv = implicitly[CanBeQueryCondition[T]]
    new ListQueryExtensionMethods(queryToExt.withFilter(f)(cv))
  }

  def filterNot[T <: Rep[_] : CanBeQueryCondition](f: E => T): ListQueryExtensionMethods[E, U] = {
    val cv = implicitly[CanBeQueryCondition[T]]
    new ListQueryExtensionMethods(queryToExt.filterNot(f)(cv))
  }

  def groupBy[K, T, G, P](f: E => K)(implicit kshape: Shape[_ <: FlatShapeLevel, K, T, G], vshape: Shape[_ <: FlatShapeLevel, E, _, P]): ListQueryExtensionMethods[(G, Query[P, U, Seq]), (T, Query[P, U, Seq])] = {
    val newQuery = queryToExt.groupBy(f)(kshape, vshape)
    new ListQueryExtensionMethods(newQuery)
  }

}*/
class PileListQueryExtensionMethods[E, U](val queryToExt: Query[E, U, Seq]) {

  def flatMap[A, B](f: E => PileListQueryWrap): PileListQueryWrap = {
    val generator = new AnonSymbol
    val aliased = queryToExt.shaped.encodeRef(Ref(generator)).value
    val fv = f(aliased)
    val slickJsonQuery = new SlickQueryBindImpl {
      override def bind[E, U](query: Query[E, U, Seq]): Query[E, U, Seq] = {
        val newQuery = fv.listQueryBind.bind(query)
        new WrappingQuery[E, U, Seq](new slick.ast.Bind(generator, queryToExt.toNode, newQuery.toNode), newQuery.shaped)
      }
    }
    PileListQueryWrap(fv.columns, slickJsonQuery)(fv.ec)
  }

  def map(ev: E => List[FPile[Option]])(implicit ec: ExecutionContext): PileListQueryWrap = {
    val generator = new AnonSymbol
    val aliased = queryToExt.shaped.encodeRef(Ref(generator)).value
    val columns = ev(aliased)
    val slickJsonQuery = new SlickQueryBindImpl {
      override def bind[E, U](query: Query[E, U, Seq]): Query[E, U, Seq] = {
        new WrappingQuery[E, U, Seq](new slick.ast.Bind(generator, queryToExt.toNode, query.toNode), query.shaped)
      }
    }
    PileListQueryWrap(columns, slickJsonQuery)
  }

  def filter[T <: Rep[_] : CanBeQueryCondition](f: E => T): PileListQueryExtensionMethods[E, U] = {
    val cv = implicitly[CanBeQueryCondition[T]]
    new PileListQueryExtensionMethods(queryToExt.filter(f)(cv))
  }

  def withFilter[T : CanBeQueryCondition](f: E => T): PileListQueryExtensionMethods[E, U] = {
    val cv = implicitly[CanBeQueryCondition[T]]
    new PileListQueryExtensionMethods(queryToExt.withFilter(f)(cv))
  }

  def filterNot[T <: Rep[_] : CanBeQueryCondition](f: E => T): PileListQueryExtensionMethods[E, U] = {
    val cv = implicitly[CanBeQueryCondition[T]]
    new PileListQueryExtensionMethods(queryToExt.filterNot(f)(cv))
  }

  def groupBy[K, T, G, P](f: E => K)(implicit kshape: Shape[_ <: FlatShapeLevel, K, T, G], vshape: Shape[_ <: FlatShapeLevel, E, _, P]): PileListQueryExtensionMethods[(G, Query[P, U, Seq]), (T, Query[P, U, Seq])] = {
    val newQuery = queryToExt.groupBy(f)(kshape, vshape)
    new PileListQueryExtensionMethods(newQuery)
  }

}