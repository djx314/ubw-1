package net.scalax.ubw.extraction.operation

import net.scalax.ubw.core._
import net.scalax.ubw.json.operation.{ AtomicValueHelper, FSomeValue }
import net.scalax.ubw.extraction.atomic.Extractor
import shapeless._

object ExtractorOperation extends AtomicValueHelper {

  type ResultType = Map[Extractor[_], Any]

  def extractor: AtomicPath => QueryTranform[ResultType] = {
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
  }

}