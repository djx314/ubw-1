package net.scalax.fsn.slick.model

import io.circe.Json
import net.scalax.fsn.core.DataPile
import net.scalax.fsn.slick.operation.{ DataWithIndex, ExecInfo3 }
import net.scalax.ubw.validate.atomic.ErrorMessage
import slick.dbio.DBIO

import scala.concurrent.{ ExecutionContext, Future }

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
  jsonGen: JsonOut,
  retrieveGen: Map[String, Json] => DBIO[Map[String, Json]],
  insertGen: Map[String, Json] => Future[Either[List[ErrorMessage], DBIO[ExecInfo3[List[DataPile]]]]],
  deleteGen: Map[String, Json] => DBIO[Int],
  updateGen: Map[String, Json] => Future[Either[List[ErrorMessage], DBIO[ExecInfo3[List[DataPile]]]]],
  //staticMany: Future[List[StaticManyUbw]]
)

case class RWInfo(
    retrieveGen: Map[String, Json] => DBIO[Map[String, Json]],
    insertGen: Map[String, Json] => Future[Either[List[ErrorMessage], DBIO[ExecInfo3[List[DataPile]]]]],
    deleteGen: Map[String, Json] => DBIO[Int],
    updateGen: Map[String, Json] => Future[Either[List[ErrorMessage], DBIO[ExecInfo3[List[DataPile]]]]],
    //staticMany: Future[List[StaticManyUbw]]
) {

  def withJsonOut(jOut: JsonOut): QueryJsonInfo = {
    QueryJsonInfo(
      jOut,
      retrieveGen,
      insertGen,
      deleteGen,
      updateGen,
      //staticMany
    )
  }

  def withJsonOutF(jOut: Future[JsonOut])(implicit ec: ExecutionContext): Future[QueryJsonInfo] = {
    jOut.map { s =>
      withJsonOut(s)
    }
  }

}