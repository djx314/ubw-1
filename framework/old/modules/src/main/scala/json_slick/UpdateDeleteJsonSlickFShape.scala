/*package net.scalax.fsn.json_slick

import io.circe.Json
import net.scalax.fsn.core.FShape
import slick.lifted._

import scala.concurrent.ExecutionContext
import scalaz.Monoid

trait UpdateDeleteSlickJsonBind {
  def bind[E, U](query: Query[E, U, Seq]): Query[E, U, Seq]
  val target: List[JsonSlickConvert]
}

class UpdateDeleteJsonSlickFShape(implicit ec: ExecutionContext) extends FShape[UpdateDeleteSlickJsonBind, IQuery] with JsonSlickMonad {

  override def encode(bind: UpdateDeleteSlickJsonBind): IQuery = {
    val jsonSlickMono = implicitly[Monoid[JsonSlickConvert]]

    val fv = bind.target.foldLeft(jsonSlickMono.zero)((s, t) => jsonSlickMono.append(s, t))

    val fvQuery = Query(fv.back.sourceCol)(fv.back.effect)
    val baseQuery: Query[fv.back.TargetColumn, fv.back.DataType, Seq] = bind.bind(fvQuery)

    val filterQueryGen =
      (data: fv.back.DataType) => fv.back.primaryGen.foldLeft(baseQuery) { (s, t) =>
        t(data).genFilter(s)
      }

    new IQuery {
      override type JsonE = fv.back.TargetColumn
      override type JsonU = fv.back.DataType
      override val jsonQuery = filterQueryGen
      override val jsonSlickConvert = (data: Map[String, Json]) => fv.convert(fv.font.effect(data))
      override val tranMany = (data: fv.back.DataType) => fv.back.staticManyMap.map { r => r.map { case (s, t) =>
        s -> t(data)
      } }
    }
  }

}*/