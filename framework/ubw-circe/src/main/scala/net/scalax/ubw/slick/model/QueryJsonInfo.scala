package net.scalax.ubw.slick.model

import io.circe.Json
import net.scalax.ubw.extraction.model.ExtractContent
import net.scalax.ubw.slick.operation.ExecInfo3
import net.scalax.ubw.validate.atomic.ErrorMessage
import slick.dbio.DBIO

import scala.concurrent.{ExecutionContext, Future}

/*case class StaticManyInfo(
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
)*/

trait ResultFromJson[T] {
  def input(v1: Map[String, Json]): Future[Either[List[ErrorMessage], DBIO[T]]]
}

object ResultFromJson {
  def apply[T](s: Map[String, Json] => Future[Either[List[ErrorMessage], DBIO[T]]]): ResultFromJson[T] = new ResultFromJson[T] {
    override def input(v1: Map[String, Json]): Future[Either[List[ErrorMessage], DBIO[T]]] = s(v1)
  }
}

case class QueryJsonInfo(
  jsonGen: JsonOut,
  retrieveGen: Map[String, Json] => DBIO[Map[String, Json]],
  insertGen: ResultFromJson[ExecInfo3[ExtractContent]],
  deleteGen: Map[String, Json] => DBIO[Int],
  updateGen: ResultFromJson[ExecInfo3[ExtractContent]],
  //staticMany: Future[List[StaticManyUbw]]
)

trait RWInfo {
  def retrieveGen: Map[String, Json] => DBIO[Map[String, Json]]
  def insertGen: ResultFromJson[ExecInfo3[ExtractContent]]
  def deleteGen: Map[String, Json] => DBIO[Int]
  def updateGen: ResultFromJson[ExecInfo3[ExtractContent]]

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