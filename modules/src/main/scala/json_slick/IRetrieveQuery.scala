package net.scalax.fsn.json_slick

import io.circe.Json
import slick.dbio._
import slick.lifted._
import slick.profile.BasicProfile

import scala.concurrent.{ExecutionContext, Future}

trait IRetrieveQuery {

  val propertyInfo: List[PropertyInfo]
  val staticMany: Future[List[StaticManyUbw]]
  type JsonE
  type JsonU
  val jsonQuery: Map[String, Json] => Query[JsonE, JsonU, Seq]
  val slickJsonConvert: JsonU => Map[String, Json]
  val tranMany: JsonU => Future[Map[String, QueryJsonInfo]]

  def retrieveResult(
    implicit
    jsonEv: Query[_, JsonU, Seq] => BasicProfile#StreamingQueryActionExtensionMethods[Seq[JsonU], JsonU],
    ec: ExecutionContext
  ): Map[String, Json] => DBIO[StaticManyInfo] = {
    (sourceData: Map[String, Json]) => {
      val targetData = jsonQuery(sourceData)
      for {
        s <- jsonEv(targetData).result.head
        tranMany <- DBIO.from(tranMany(s))
      } yield {
        StaticManyInfo(propertyInfo, slickJsonConvert(s), tranMany)
      }
    }
  }

}