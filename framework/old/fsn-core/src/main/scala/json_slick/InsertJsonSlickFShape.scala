/*package net.scalax.fsn.json_slick

import io.circe.Json
import net.scalax.fsn.core.FShape
import slick.lifted._

import scala.concurrent.{ExecutionContext, Future}
import scalaz.Monoid

trait InsertJsonSlickBind {
  def bind[E, U](query: Query[E, U, Seq]): Query[E, U, Seq]
  val target: List[(JsonSlickConvert, Boolean)]
}

class InsertJsonSlickFShape(implicit ec: ExecutionContext) extends FShape[InsertJsonSlickBind, InsertQuery] with JsonSlickMonad {

  override def encode(bind: InsertJsonSlickBind): InsertQuery = {

    val fv = bind.target.foldLeft(SlickJsonInConvertMonoId.zero) { case (toAppend, (convert, isAutoInc)) =>
      if (isAutoInc) {
        //If this column is autoInc
        SlickJsonInConvertMonoId.appendFirst(toAppend, convert)
      } else {
        SlickJsonInConvertMonoId.appendSecond(toAppend, convert)
      }
    }

    val fieldQuery = Query(fv.second.sourceCol)(fv.second.effect)
    val baseFieldQuery: Query[fv.second.TargetColumn, fv.second.DataType, Seq] = bind.bind(fieldQuery)
    val idQuery = Query(fv.first.sourceCol)(fv.first.effect)
    val baseIdQuery: Query[fv.first.TargetColumn, fv.first.DataType, Seq] = bind.bind(idQuery)
    new InsertQuery {
      override type InsertTarget = fv.second.TargetColumn
      override type IdTarget = fv.first.TargetColumn
      override type InsertField = fv.second.DataType
      override type IdField = fv.first.DataType
      override val baseQuery = baseFieldQuery
      override val returningQuery = baseIdQuery
      override val slickJsonConvert = (data: Map[String, Json]) => fv.convert(fv.font.effect(data))
      override val tranMany: ((IdField, InsertField)) => Future[Map[String, QueryJsonInfo]] = s => {
        for {
          firstMap <- fv.first.staticManyMap
          secondMap <- fv.second.staticManyMap
        } yield {
          firstMap.map { case (key, value) => key -> value(s._1) } ++ secondMap.map { case (key, value) => key -> value(s._2) }
        }
      }
    }
  }

}*/