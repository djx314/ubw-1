package net.scalax.fsn.slick.model

import io.circe.Json
import slick.dbio.DBIO

import scala.concurrent.Future

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