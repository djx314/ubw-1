package net.scalax.fsn.json.atomic

import io.circe.Decoder
import net.scalax.fsn.core.FAtomic
import net.scalax.fsn.slick.helpers.FilterModel

trait JsonReader[E] extends FAtomic[E] {

  type JsonType
  type DataType = E

  val reader: Decoder[JsonType]
  val convert: JsonType => DataType

}

trait SlickCompare[E] extends FAtomic[E] {
  type DataType = E
  val reader: Decoder[FilterModel[E]]
}

trait SlickCompareData[E] extends FAtomic[E] {
  val compare: FilterModel[E]
}