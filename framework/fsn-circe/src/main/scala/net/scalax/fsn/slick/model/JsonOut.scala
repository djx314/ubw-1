package net.scalax.fsn.slick.model

import io.circe.Json

import slick.dbio.DBIO
import scala.concurrent.ExecutionContext

case class JsonView(properties: List[SelectProperty], data: List[Map[String, Json]], sum: Option[Int])

case class JsonOut(properties: List[SelectProperty], data: SlickParam => ResultWrap) {
  def toView(param: SlickParam)(implicit ec: ExecutionContext): DBIO[JsonView] = {
    data(param).resultAction.map(t => JsonView(properties, t.data, t.sum))
  }

  def statement(param: SlickParam): List[String] = data(param).statements
}

case class ResultCollection(data: List[Map[String, Json]], sum: Option[Int])
case class ResultWrap(resultAction: DBIO[ResultCollection], statements: List[String])

case class ListAnyCollection(data: List[List[Option[Any]]], sum: Option[Int])
case class ListAnyWrap(resultAction: DBIO[ListAnyCollection], statements: List[String])