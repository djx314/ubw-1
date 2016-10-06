package net.scalax.fsn.mix.slickbase

import net.scalax.fsn.slick.helpers.{SlickQueryBindImpl, SlickUtils}
import slick.ast.{AnonSymbol, Ref}
import slick.lifted._
import slick.relational.RelationalProfile

//TODO 看日后能否去掉 table 的绑定
class CrudQueryExtensionMethods[E <: RelationalProfile#Table[_], U](val queryToExt: Query[E, U, Seq]) {

  def flatMap[A, B](f: E => QueryWrap): QueryWrap = {
    val generator = new AnonSymbol
    val aliased = queryToExt.shaped.encodeRef(Ref(generator)).value
    val fv = f(aliased)
    val slickJsonQuery = new SlickQueryBindImpl {
      override def bind[E, U](query: Query[E, U, Seq]): Query[E, U, Seq] = {
        new WrappingQuery[E, U, Seq](new slick.ast.Bind(generator, queryToExt.toNode, query.toNode), query.shaped)
      }
    }
    val deleteWrap =
      (SlickUtils.getTableIdFromTable(aliased) -> slickJsonQuery) :: fv.binds

    QueryWrap(deleteWrap, fv.listQueryWrap)(fv.ec)
  }

  def map[A, B](f: E => ListQueryWrap): QueryWrap = {
    val generator = new AnonSymbol
    val aliased = queryToExt.shaped.encodeRef(Ref(generator)).value
    val fv = f(aliased)
    val slickJsonQuery = new SlickQueryBindImpl {
      override def bind[E, U](query: Query[E, U, Seq]): Query[E, U, Seq] = {
        new WrappingQuery[E, U, Seq](new slick.ast.Bind(generator, queryToExt.toNode, query.toNode), query.shaped)
      }
    }
    val deleteWrap =
      List(SlickUtils.getTableIdFromTable(aliased) -> slickJsonQuery)

    QueryWrap(deleteWrap, fv)(fv.ec)
  }

  def filter[T <: Rep[_] : CanBeQueryCondition](f: E => T): CrudQueryExtensionMethods[E, U] = {
    val cv = implicitly[CanBeQueryCondition[T]]
    new CrudQueryExtensionMethods(queryToExt.filter(f)(cv))
  }

  def withFilter[T : CanBeQueryCondition](f: E => T): CrudQueryExtensionMethods[E, U] = {
    val cv = implicitly[CanBeQueryCondition[T]]
    new CrudQueryExtensionMethods(queryToExt.withFilter(f)(cv))
  }

  def filterNot[T <: Rep[_] : CanBeQueryCondition](f: E => T): CrudQueryExtensionMethods[E, U] = {
    val cv = implicitly[CanBeQueryCondition[T]]
    new CrudQueryExtensionMethods(queryToExt.filterNot(f)(cv))
  }

}