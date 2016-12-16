package net.scalax.fsn.parameter.atomic

import io.circe.{Encoder, Json}

import scala.concurrent.{ExecutionContext, Future}
import io.circe.generic.auto._
import io.circe.syntax._

trait FCompAbs {
  self =>

  type ParamType
  val extParam: ParamType
  val encoder: Encoder[ParamType]
  val componentName: String
  val compData: Future[Json]

  def toContent(key: String)(implicit ec: ExecutionContext): Future[CompContentAbs] = {
    compData.map { s =>
      CompContent(key, self.componentName, self.extParam, s)(self.encoder)
    }
  }

}

trait CompContentAbs {

  type ParamType
  val extParam: ParamType
  val encoder: Encoder[ParamType]
  val compKey: String
  val compName: String
  val data: Json

  def toDataJson: Json = {
    implicit val jsonDataEncoder = this.encoder
    CompContent(compKey, compName, extParam, data)(encoder).asJson
  }

}

case class CompContent[T](compKey: String, compName: String, extParam: T, data: Json)(implicit val encoder: Encoder[T]) extends CompContentAbs {
  override type ParamType = T
}

trait DataMapGen[P] {
  def apply(s: P): List[Future[CompContentAbs]]
}