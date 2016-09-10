package net.scalax.fsn.slick_common

import io.circe.Json
import org.xarcher.cpoi.CellData
import slick.dbio.DBIO

import scala.concurrent.ExecutionContext

@io.circe.generic.JsonCodec
case class PropertyInfo(
  property: String,
  typeName: String,
  inRetrieve: Boolean,
  canOrder: Boolean,
  isDefaultDesc: Boolean//,
  //selectRender: String,
  //retrieveRender: String,
  //inputRender: String
)

@io.circe.generic.JsonCodec
case class JsonView(properties: List[PropertyInfo], data: List[Map[String, Json]], sum: Int)

case class JsonOut(properties: List[PropertyInfo], data: SlickParam => DBIO[(List[Map[String, Json]], Int)]) {
  def toView(param: SlickParam)(implicit ec: ExecutionContext): DBIO[JsonView] = {
    data(param).map(t => JsonView(properties, t._1, t._2))
  }
}
case class PoiOut(properties: List[PropertyInfo], data: SlickParam => DBIO[(List[Map[String, CellData[_]]], Int)])

case class SlickRange(drop: Int, take: Option[Int])
case class SlickPage(pageIndex: Int, pageSize: Int)
case class ColumnOrder(columnName: String, isDesc: Boolean)

case class SlickParam(orders: List[ColumnOrder] = Nil, range: Option[SlickRange] = None, page: Option[SlickPage] = None)
