package net.scalax.fsn.shapes

import net.scalax.fsn.core.FShape
import net.scalax.fsn.json_slick._
import net.scalax.fsn.slick_json.{JsonQuery, SlickJsonBind, SlickJsonConvert, SlickJsonFShape}
import net.scalax.fsn.slick_poi.{PoiQuery, SlickPoiBind, SlickPoiConvert, SlickPoiFShape}

import scala.concurrent.ExecutionContext

trait Implicits {

  implicit def jsonQueyShape: FShape[SlickJsonBind, JsonQuery] = new SlickJsonFShape {}
  implicit def poiQueyShape: FShape[SlickPoiBind, PoiQuery] = new SlickPoiFShape {}
  implicit def insertJsonSlickShape(implicit ec: ExecutionContext): FShape[InsertJsonSlickBind, InsertQuery] = new InsertJsonSlickFShape()(ec)
  implicit def iRetrieveJsonSlickShape(implicit ec: ExecutionContext): FShape[IRetrieveJsonSlickBind, IRetrieveQuery] = new IRetrieveJsonSlickFShape()(ec)
  implicit def updateDeleteJsonSlickShape(implicit ec: ExecutionContext): FShape[UpdateDeleteSlickJsonBind, IQuery] = new UpdateDeleteJsonSlickFShape()(ec)

}