package net.scalax.ubw.database.test

import io.circe.Json
import io.circe.syntax._

import scala.concurrent.ExecutionContext.Implicits.global
import slick.jdbc.H2Profile.api._

object Sample03 {

  def toJSON(query: Query[FriendTable, FriendTable#TableElementType, Seq]): DBIO[Seq[Map[String, Json]]] = {
    query.map { s =>
      (s.name, s.grade, s.age)
    }.result.map { list =>
      list.map { s =>
        Map(
          "name" -> s._1.asJson,
          "grade" -> s._2.asJson,
          "age" -> s._3.asJson
        )
      }
    }
  }

}