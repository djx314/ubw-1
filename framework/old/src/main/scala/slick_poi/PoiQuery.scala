package net.scalax.fsn.slick_poi

import net.scalax.fsn.slick.model.{ColumnOrder, PoiOut, SelectProperty, SlickParam}
import net.scalax.fsn.slick_common.CommonResult
import org.xarcher.cpoi.CellData
import slick.basic.BasicProfile
import slick.dbio._
import slick.lifted._

import scala.concurrent.ExecutionContext

trait PoiQuery {

  type PoiE
  type PoiU

  val uQuery: Query[PoiE, PoiU, Seq]
  val render: PoiU => Map[String, CellData[_]]
  val properties: List[SelectProperty]
  val sortMap: Map[String, PoiE => ColumnOrdered[_]]

  def poiResult(
    implicit
    streamEv: Query[_, PoiU, Seq] => BasicProfile#StreamingQueryActionExtensionMethods[Seq[PoiU], PoiU],
    repToDBIO: Rep[Int] => BasicProfile#QueryActionExtensionMethods[Int, NoStream],
    ec: ExecutionContext
  ): PoiOut = {
    poiResult(Nil)
  }

  def poiResult(orderColumn: String, isDesc: Boolean = true)(
    implicit
    streamEv: Query[_, PoiU, Seq] => BasicProfile#StreamingQueryActionExtensionMethods[Seq[PoiU], PoiU],
    repToDBIO: Rep[Int] => BasicProfile#QueryActionExtensionMethods[Int, NoStream],
    ec: ExecutionContext
  ): PoiOut = {
    poiResult(List(ColumnOrder(orderColumn, isDesc)))
  }

  def poiResult(defaultOrders: List[ColumnOrder])(
    implicit
    streamEv: Query[_, PoiU, Seq] => BasicProfile#StreamingQueryActionExtensionMethods[Seq[PoiU], PoiU],
    repToDBIO: Rep[Int] => BasicProfile#QueryActionExtensionMethods[Int, NoStream],
    ec: ExecutionContext
  ): PoiOut = {
      PoiOut(properties, (slickParam: SlickParam) => CommonResult.commonResult(defaultOrders, uQuery, render, sortMap/*, properties*/).apply(slickParam)/*.map(PoiOut.tupled)*/)
  }

}