package net.scalax.fsn.json

import io.circe.{Decoder, Encoder}
import net.scalax.fsn.core.FAtomic
import scala.reflect.runtime.universe._

trait JsonReader[E] extends FAtomic[E] {

  type JsonType
  type DataType = E

  val reader: Decoder[JsonType]
  val convert: JsonType => DataType

}

trait JsonWriter[E] extends FAtomic[E] {

  type JsonType
  type DataType = E
  val writer: Encoder[JsonType]
  val convert: DataType => JsonType
  val typeTag: WeakTypeTag[JsonType]

}