package net.scalax.fsn.model

import io.circe.Json
import org.xarcher.cpoi.CellData
import slick.dbio.DBIO

import scala.concurrent.ExecutionContext

case class SelectProperty(
  property: String,
  typeName: String,
  inRetrieve: Boolean,
  canOrder: Boolean,
  isDefaultDesc: Boolean
)

case class JsonView(properties: List[SelectProperty], data: List[Map[String, Json]], sum: Int)

case class JsonOut(properties: List[SelectProperty], data: SlickParam => DBIO[(List[Map[String, Json]], Int)]) {
  def toView(param: SlickParam)(implicit ec: ExecutionContext): DBIO[JsonView] = {
    data(param).map(t => JsonView(properties, t._1, t._2))
  }
}
case class PoiOut(properties: List[SelectProperty], data: SlickParam => DBIO[(List[Map[String, CellData[_]]], Int)])