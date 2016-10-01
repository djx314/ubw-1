package net.scalax.fsn.json.atomic

import io.circe.Decoder
import net.scalax.fsn.core.FAtomic

trait JsonReader[E] extends FAtomic[E] {

  type JsonType
  type DataType = E

  val reader: Decoder[JsonType]
  val convert: JsonType => DataType

}