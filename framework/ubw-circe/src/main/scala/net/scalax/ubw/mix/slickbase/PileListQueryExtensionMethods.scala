package net.scalax.ubw.mix.slickbase

import net.scalax.ubw.core.Pile
import net.scalax.ubw.slick.helpers.SlickQueryBindImpl
import net.scalax.ubw.slick.model.{ ColumnOrder, SlickPage, SlickParam, SlickRange }
import slick.ast.{ AnonSymbol, Ref }
import slick.lifted._

import scala.concurrent.ExecutionContext

case class PileListQueryWrap(
    columns: List[Pile],
    listQueryBind: SlickQueryBindImpl,
    slickParam: SlickParam = SlickParam()
)(implicit val ec: ExecutionContext) {

  def page(page: SlickPage): PileListQueryWrap = {
    this.copy(slickParam = this.slickParam.copy(page = Option(page)))
  }

  def pageIndex(index: Int): PileListQueryWrap = {
    this.copy(slickParam = this.slickParam.copy(page = Option(SlickPage(pageIndex = index, pageSize = this.slickParam.page.map(_.pageSize).getOrElse(10)))))
  }

  def pageSize(size: Int): PileListQueryWrap = {
    this.copy(slickParam = this.slickParam.copy(page = Option(SlickPage(pageIndex = this.slickParam.page.map(_.pageIndex).getOrElse(0), pageSize = size))))
  }

  def range(range: SlickRange): PileListQueryWrap = {
    this.copy(slickParam = this.slickParam.copy(range = Option(range)))
  }

  def drop(drop: Int): PileListQueryWrap = {
    this.copy(slickParam = this.slickParam.copy(range = Option(SlickRange(drop = drop, take = this.slickParam.range.flatMap(_.take)))))
  }

  def take(take: Int): PileListQueryWrap = {
    this.copy(slickParam = this.slickParam.copy(range = Option(SlickRange(drop = this.slickParam.range.map(_.drop).getOrElse(0), take = Option(take)))))
  }

  def withOrders(orders: List[ColumnOrder]): PileListQueryWrap = {
    this.copy(slickParam = this.slickParam.copy(orders = orders))
  }

  def withOrder(columnName: String, isDesc: Boolean = false): PileListQueryWrap = {
    this.copy(slickParam = this.slickParam.copy(orders = List(ColumnOrder(columnName, isDesc))))
  }

  def addOrders(orders: List[ColumnOrder]): PileListQueryWrap = {
    this.copy(slickParam = this.slickParam.copy(orders = orders ::: this.slickParam.orders))
  }

  def addOrder(columnName: String, isDesc: Boolean = false): PileListQueryWrap = {
    this.copy(slickParam = this.slickParam.copy(orders = this.slickParam.orders ::: List(ColumnOrder(columnName, isDesc))))
  }

}

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

  def map(ev: E => List[Pile])(implicit ec: ExecutionContext): PileListQueryWrap = {
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

  def filter[T <: Rep[_]: CanBeQueryCondition](f: E => T): PileListQueryExtensionMethods[E, U] = {
    val cv = implicitly[CanBeQueryCondition[T]]
    new PileListQueryExtensionMethods(queryToExt.filter(f)(cv))
  }

  def withFilter[T: CanBeQueryCondition](f: E => T): PileListQueryExtensionMethods[E, U] = {
    val cv = implicitly[CanBeQueryCondition[T]]
    new PileListQueryExtensionMethods(queryToExt.withFilter(f)(cv))
  }

  def filterNot[T <: Rep[_]: CanBeQueryCondition](f: E => T): PileListQueryExtensionMethods[E, U] = {
    val cv = implicitly[CanBeQueryCondition[T]]
    new PileListQueryExtensionMethods(queryToExt.filterNot(f)(cv))
  }

  def groupBy[K, T, G, P](f: E => K)(implicit kshape: Shape[_ <: FlatShapeLevel, K, T, G], vshape: Shape[_ <: FlatShapeLevel, E, _, P]): PileListQueryExtensionMethods[(G, Query[P, U, Seq]), (T, Query[P, U, Seq])] = {
    val newQuery = queryToExt.groupBy(f)(kshape, vshape)
    new PileListQueryExtensionMethods(newQuery)
  }

}