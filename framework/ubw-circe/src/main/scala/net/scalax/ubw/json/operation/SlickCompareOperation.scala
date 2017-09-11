/*package net.scalax.ubw.json.operation

import io.circe.Json
import net.scalax.ubw.common.atomic.FProperty
import net.scalax.ubw.core._
import net.scalax.ubw.json.atomic.{ CompareToStringConvert, SlickCompare }
import net.scalax.ubw.slick.helpers.{ FilterModel, FilterModelHelper }
import shapeless._

object SlickCompareOperation extends FilterModelHelper {

  val unfullReadCompareGen = Pile.transformTreeList {
    new AtomicQuery(_) {
      val aa = withRep(needAtomic[SlickCompare] :: needAtomic[FProperty] :: needAtomic[CompareToStringConvert] :: FANil)
        .mapTo {
          case (jsonReader :: property :: compareCv :: HNil, _) =>
            val tran: Map[String, Json] => AtomicValueImpl[path.DataType] = { sourceData: Map[String, Json] =>
              sourceData.get(property.proName) match {
                case Some(json) =>
                  json.as[FilterModel[jsonReader.DataType]](jsonReader.reader) match {
                    case Right(data) =>
                      val resultData = if ((compareCv.strCv eq null) && (compareCv.optStrCv eq null)) {
                        data
                      } else if (!(compareCv.strCv eq null)) {
                        data.copy(eq = data.eq.map(compareCv.strCv.to).filterNot(_.isEmpty).map(compareCv.strCv.from))
                      } else {
                        data.copy(eq = data.eq.map(compareCv.optStrCv.to).map(_.filterNot(_.isEmpty)).filterNot(_.isEmpty).map(compareCv.optStrCv.from))
                      }
                      set(resultData)
                    case _ =>
                      emptyValue[jsonReader.DataType]
                  }
                case None =>
                  emptyValue[jsonReader.DataType]
              }
            }

            tran: (Map[String, Json] => AtomicValue)
        }
    }.aa
  } { readlerList =>
    { sourceData: Map[String, Json] =>
      readlerList.map(_.apply(sourceData))
    }
  }

}*/ 