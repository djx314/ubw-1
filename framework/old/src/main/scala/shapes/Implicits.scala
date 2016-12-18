/*package net.scalax.fsn.shapes

import net.scalax.fsn.core.FShape
import net.scalax.fsn.json.atomic_slick._
import net.scalax.fsn.slick_json.{JsonQuery, SlickJsonBind, SlickJsonFShape}
import net.scalax.fsn.slick_poi.{PoiQuery, SlickPoiBind, SlickPoiFShape}

import scala.concurrent.ExecutionContext

trait Implicits {

  implicit def jsonQueyShape: FShape[SlickJsonBind, JsonQuery] = new SlickJsonFShape {}
  implicit def poiQueyShape: FShape[SlickPoiBind, PoiQuery] = new SlickPoiFShape {}

}*/