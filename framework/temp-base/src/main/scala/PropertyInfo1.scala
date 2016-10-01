package net.scalax.fsn.slick.model

import io.circe.Json
import slick.dbio.DBIO
import slick.lifted.{CanBeQueryCondition, Query, Rep}

import scala.concurrent.Future

case class SlickRange(drop: Int, take: Option[Int])
case class SlickPage(pageIndex: Int, pageSize: Int)
case class ColumnOrder(columnName: String, isDesc: Boolean)

case class SlickParam(orders: List[ColumnOrder] = Nil, range: Option[SlickRange] = None, page: Option[SlickPage] = None)

trait FilterWrapper[E] {
  type Target <: Rep[_]
  val condition: CanBeQueryCondition[Target]
  val convert: E => Target

  def genFilter[U](query: Query[E, U, Seq]): Query[E, U, Seq] = {
    query.filter(data => convert(data))(condition)
  }
}

case class RWProperty(
                         property: String,
                         typeName: String,
                         inRetrieve: Boolean,
                         //canOrder: Boolean,
                         //isDefaultDesc: Boolean,
                         isAutoInc: Boolean,
                         isPrimaryKey: Boolean = false
                       )

case class StaticManyInfo(
                           propertyInfo: List[RWProperty],
                           model: Map[String, Json],
                           many: Map[String, QueryJsonInfo]
                         )

case class UpdateStaticManyInfo(
                                 effectRows: Int,
                                 many: Map[String, QueryJsonInfo]
                               )

case class StaticManyGen[T](
                             //model 的属性名称
                             proName: String,
                             //关联表的主表 id 字段
                             //masterIdField: String,
                             //关联表的从表 id 字段
                             slaveryIdField: String,
                             gen: T => QueryJsonInfo,
                             ubwGen: JsonOut
                           )

case class StaticManyUbw(
                          //model 的属性名称
                          proName: String,
                          //关联表的主表 id 字段
                          masterIdField: String,
                          //关联表的从表 id 字段
                          slaveryIdField: String,
                          ubwGen: JsonOut
                        )

case class QueryJsonInfo(
                          properties: List[RWProperty],
                          jsonGen: JsonOut,
                          retrieveGen: Map[String, Json] => DBIO[StaticManyInfo],
                          insertGen: Map[String, Json] => DBIO[UpdateStaticManyInfo],
                          deleteGen: Map[String, Json] => DBIO[Int],
                          updateGen: Map[String, Json] => DBIO[UpdateStaticManyInfo],
                          staticMany: Future[List[StaticManyUbw]]
                        )