package net.scalax.fsn.slick.model

import io.circe.Json
import slick.dbio.DBIO

import scala.concurrent.ExecutionContext

case class JsonView(data: List[Map[String, Json]], sum: Option[Int])

case class JsonOut(data: ListAnyWrap3333[Map[String, Json]]) {
  def toView(implicit ec: ExecutionContext): DBIO[JsonView] = {
    data.resultAction.map(t => JsonView(t.data, t.sum))
  }

  def statement: List[String] = data.statements
}
/*case class ResultCollection(data: List[Map[String, Json]], sum: Option[Int])
case class ResultWrap(resultAction: DBIO[ListAnyWrap3333[Map[String, Json]]], statements: List[String])*/
/*case class ListAnyCollection(data: List[List[Option[Any]]], sum: Option[Int])
case class ListAnyWrap(resultAction: DBIO[ListAnyCollection], statements: List[String])*/
/*case class ListAnyCollection1111(data: List[List[AtomicValue]], sum: Option[Int])
case class ListAnyWrap1111(resultAction: DBIO[ListAnyCollection1111], statements: List[String])

case class ListAnyCollection2222(data: List[List[DataPile]], sum: Option[Int])
case class ListAnyWrap2222(resultAction: DBIO[ListAnyCollection2222], statements: List[String])*/
case class ListAnyCollection3333[T](data: List[T], sum: Option[Int])
case class ListAnyWrap3333[T](resultAction: DBIO[ListAnyCollection3333[T]], statements: List[String])