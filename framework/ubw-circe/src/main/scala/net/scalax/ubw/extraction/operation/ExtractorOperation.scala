package net.scalax.ubw.extraction.operation

import net.scalax.ubw.core._
import net.scalax.ubw.json.operation.{ AtomicValueHelper, FSomeValue }
import net.scalax.ubw.extraction.atomic.Extractor
import net.scalax.ubw.extraction.model.ExtractContent
import shapeless._

object ExtractorOperation extends AtomicValueHelper {

  type ResultType = Map[Extractor[_], Any]

  def extractor: PileFilter[ExtractContent] = PileFilter {
    new AtomicQuery(_) {
      val aa = withRep(needAtomic[Extractor] :: FANil)
        .mapTo {
          case (extractpr :: HNil, data) =>
            val dataOpt = data match {
              case FSomeValue(s) => Option(s)
              case AtomicValueImpl.Zero() => Option.empty[data.DataType]
            }
            dataOpt.map(data => Map(extractpr -> data): ResultType).getOrElse(Map.empty[Extractor[_], Any])
        }
    }.aa
  } { results =>
    ExtractContent(results match {
      case Nil => Map.empty[Extractor[_], Any]
      case list => list.reduce(_ ++ _)
    })
  }

}