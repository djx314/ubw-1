package net.scalax.fsn.slick_json

import io.circe.Json
import net.scalax.fsn.model.{ColumnOrder, SlickParam}
import net.scalax.fsn.slick_common._
import slick.basic.BasicProfile
import slick.dbio._
import slick.lifted._

import scala.concurrent.ExecutionContext

trait JsonQuery {

  type JsonE
  type JsonU

  val uQuery: Query[JsonE, JsonU, Seq]
  val render: JsonU => Map[String, Json]
  val properties: List[PropertyInfo]
  val sortMap: Map[String, JsonE => ColumnOrdered[_]]

  def jsonResult(
    implicit
    jsonEv: Query[_, JsonU, Seq] => BasicProfile#StreamingQueryActionExtensionMethods[Seq[JsonU], JsonU],
    repToDBIO: Rep[Int] => BasicProfile#QueryActionExtensionMethods[Int, NoStream],
    ec: ExecutionContext
  ): JsonOut = {
    jsonResult(Nil)
  }

  def jsonResult(orderColumn: String, isDesc: Boolean = true)(
    implicit
    jsonEv: Query[_, JsonU, Seq] => BasicProfile#StreamingQueryActionExtensionMethods[Seq[JsonU], JsonU],
    repToDBIO: Rep[Int] => BasicProfile#QueryActionExtensionMethods[Int, NoStream],
    ec: ExecutionContext
  ): JsonOut = {
    jsonResult(List(ColumnOrder(orderColumn, isDesc)))
  }

  def jsonResult(defaultOrders: List[ColumnOrder])(
    implicit
    jsonEv: Query[_, JsonU, Seq] => BasicProfile#StreamingQueryActionExtensionMethods[Seq[JsonU], JsonU],
    repToDBIO: Rep[Int] => BasicProfile#QueryActionExtensionMethods[Int, NoStream],
    ec: ExecutionContext
  ): JsonOut = {
      JsonOut(properties = properties, data = (slickParam: SlickParam) => CommonResult.commonResult(defaultOrders, uQuery, render, sortMap/*, properties*/).apply(slickParam))
  }

}