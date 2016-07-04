package net.scalax.fsn.json_slick

import io.circe.Json
import slick.dbio._
import slick.jdbc.JdbcActionComponent
import slick.lifted._
import slick.relational.RelationalProfile

import scala.concurrent.{ExecutionContext, Future}

trait IQuery {

  type JsonE
  type JsonU
  val jsonQuery: JsonU => Query[JsonE, JsonU, Seq]
  val jsonSlickConvert: Map[String, Json] => JsonU
  val tranMany: JsonU => Future[Map[String, QueryJsonInfo]]

  def deleteResult(
                    implicit
                    conV: Query[RelationalProfile#Table[_], _, Seq] => JdbcActionComponent#DeleteActionExtensionMethods,
                    ec: ExecutionContext
                  ): Map[String, Json] => DBIO[Int] = {
    (sourceData: Map[String, Json]) => {
      val targetData = jsonSlickConvert(sourceData)
      conV(jsonQuery(targetData).asInstanceOf[Query[RelationalProfile#Table[_], _, Seq]]).delete
    }
  }

  def updateResult(
                    implicit
                    conV: Query[_, JsonU, Seq] => JdbcActionComponent#UpdateActionExtensionMethods[JsonU],
                    ec: ExecutionContext
                  ): Map[String, Json] => DBIO[UpdateStaticManyInfo] = {
    (sourceData: Map[String, Json]) => {
      val targetData = jsonSlickConvert(sourceData)
      conV(jsonQuery(targetData)).update(targetData)
      for {
        s <- conV(jsonQuery(targetData)).update(targetData)
        tranMany <- DBIO.from(tranMany(targetData))
      } yield {
        UpdateStaticManyInfo(s, tranMany)
      }
    }
  }

}

trait InsertQuery {

  type InsertTarget
  type IdTarget
  type InsertField
  type IdField
  val baseQuery: Query[InsertTarget, InsertField, Seq]
  val returningQuery: Query[IdTarget, IdField, Seq]
  //val jsonQuery: JdbcActionComponent#ReturningInsertActionComposer[InsertField, IdField]
  val slickJsonConvert: Map[String, Json] => InsertField
  val tranMany: ((IdField, InsertField)) => Future[Map[String, QueryJsonInfo]]

  def insertResult(
                    implicit
                    ev: Query[_, InsertField, Seq] => JdbcActionComponent#InsertActionExtensionMethods[InsertField],
                    ec: ExecutionContext
                  ): Map[String, Json] => DBIO[UpdateStaticManyInfo] = {
    (sourceData: Map[String, Json]) => {
      val targetData = slickJsonConvert(sourceData)
      val returnData = ev(baseQuery).returning(returningQuery).+=(targetData)
      returnData.flatMap { data =>
        DBIO.from(tranMany(data -> targetData)).map(s => UpdateStaticManyInfo(1, s))
      }
    }
  }

}

case class IQueryWrapper(retrieve: IRetrieveQuery, insert: InsertQuery, delete: IQuery, update: IQuery, staticManyGen: Future[List[StaticManyUbw]]) {

  private type JsonType = Map[String, Json]

  /*def insertResult(
    implicit
    ev: Query[_, insert.InsertField, Seq] => JdbcActionComponent#InsertActionExtensionMethods[insert.InsertField],
    ec: ExecutionContext
  ): JsonType => DBIO[UpdateStaticManyInfo] = {
    (sourceData: JsonType) => {
      val targetData = insert.slickJsonConvert(sourceData)
      val returnData = ev(insert.baseQuery).returning(insert.returningQuery).+=(targetData)
      returnData.flatMap { data =>
        DBIO.from(insert.tranMany(data -> targetData)).map(s => UpdateStaticManyInfo(1, s))
      }
    }
  }*/

  /*def retrieveResult(
    implicit
    jsonEv: Query[_, retrieve.JsonU, Seq] => BasicProfile#StreamingQueryActionExtensionMethods[Seq[retrieve.JsonU], retrieve.JsonU],
    ec: ExecutionContext
  ): Map[String, Json] => DBIO[StaticManyInfo] = {
    (sourceData: Map[String, Json]) => {
      val targetData = retrieve.jsonQuery(sourceData)
      for {
        s <- jsonEv(targetData).result.head
        tranMany <- DBIO.from(retrieve.tranMany(s))
      } yield {
        StaticManyInfo(retrieve.propertyInfo, retrieve.slickJsonConvert(s), tranMany)
      }
    }
  }*/
  /*def deleteResult(
    implicit
    conV: Query[RelationalProfile#Table[_], _, Seq] => JdbcActionComponent#DeleteActionExtensionMethods,
    ec: ExecutionContext
  ): JsonType => DBIO[Int] = {
    (sourceData: JsonType) => {
      val targetData = delete.jsonSlickConvert(sourceData)
      conV(delete.jsonQuery(targetData).asInstanceOf[Query[RelationalProfile#Table[_], _, Seq]]).delete
    }
  }

  def updateResult(
    implicit
    conV: Query[_, update.JsonU, Seq] => JdbcActionComponent#UpdateActionExtensionMethods[update.JsonU],
    ec: ExecutionContext
  ): JsonType => DBIO[UpdateStaticManyInfo] = {
    (sourceData: JsonType) => {
      val targetData = update.jsonSlickConvert(sourceData)
      conV(update.jsonQuery(targetData)).update(targetData)
      for {
        s <- conV(update.jsonQuery(targetData)).update(targetData)
        tranMany <- DBIO.from(update.tranMany(targetData))
      } yield {
        UpdateStaticManyInfo(s, tranMany)
      }
    }
  }*/
}