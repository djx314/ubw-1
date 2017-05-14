package net.scalax.fsn.json.operation

import io.circe.Json
import io.circe.syntax._
import net.scalax.fsn.common.atomic.{ DefaultValue, FProperty }
import net.scalax.fsn.core._
import net.scalax.fsn.json.atomic.{ JsonReader, JsonWriter, SlickCompare }
import net.scalax.fsn.slick.helpers.{ FilterModel, FilterModelHelper }
import shapeless._

object SlickCompareOperation extends FilterModelHelper {

  val unfullReadCompareGen = FPile.transformTreeList {
    new FAtomicQuery(_) {
      val aa = withRep(needAtomic[SlickCompare] :: needAtomic[FProperty] :: FANil)
        .mapTo {
          case (jsonReader :: property :: HNil, _) =>
            val tran: Map[String, Json] => FAtomicValueImpl[path.DataType] = { sourceData: Map[String, Json] =>
              sourceData.get(property.proName) match {
                case Some(json) =>
                  json.as[FilterModel[jsonReader.DataType]](jsonReader.reader) match {
                    case Right(data) =>
                      set(data)
                    case _ =>
                      emptyValue[jsonReader.DataType]
                  }
                case None =>
                  emptyValue[jsonReader.DataType]
              }
            }

            tran: (Map[String, Json] => FAtomicValue)
        }
    }.aa
  } { readlerList =>
    { sourceData: Map[String, Json] =>
      readlerList.map(_.apply(sourceData))
    }
  }

}