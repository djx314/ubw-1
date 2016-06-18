package net.scalax.fsn.json_slick

import io.circe.Json
import net.scalax.fsn.core.FShape
import slick.lifted._

import scala.concurrent.ExecutionContext
import scalaz.Monoid

trait IRetrieveJsonSlickBind {
  def bind[E, U](query: Query[E, U, Seq]): Query[E, U, Seq]
  val target: List[PJsonSlickConvert]
}

class IRetrieveJsonSlickFShape(implicit ec: ExecutionContext) extends FShape[IRetrieveJsonSlickBind, IRetrieveQuery] with PJsonSlickMonad {

  override def encode(bind: IRetrieveJsonSlickBind): IRetrieveQuery = {
    val jsonSlickMono = implicitly[Monoid[PJsonSlickConvert]]

    val fv = bind.target.foldLeft(jsonSlickMono.zero)((s, t) => jsonSlickMono.append(s, t))

    val fvQuery = Query(fv.middle.sourceCol)(fv.middle.effect)
    val baseQuery: Query[fv.middle.TargetColumn, fv.middle.DataType, Seq] = bind.bind(fvQuery)

    val filterQueryGen =
      (data: Map[String, Json]) => fv.middle.primaryGen.foldLeft(baseQuery) { (s, t) =>
        t(fv.convertJtoS(fv.font.effect(data))).genFilter(s)
      }

    new IRetrieveQuery {
      override val propertyInfo = fv.propertyInfo
      override val staticMany = fv.staticMany
      override type JsonE = fv.middle.TargetColumn
      override type JsonU = fv.middle.DataType
      override val jsonQuery = filterQueryGen
      override val slickJsonConvert = (data: fv.middle.DataType) => fv.back.effect(fv.convertStoJ(data))
      override val tranMany = (data: fv.middle.DataType) => fv.middle.staticManyMap.map { r => r.map { case (s, t) =>
        s -> t(data)
      } }
    }
  }

}