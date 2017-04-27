package net.scalax.fsn.json.atomic

import io.circe.Encoder
import net.scalax.fsn.core.FAtomic
import scala.reflect.runtime.universe._

trait JsonWriter[E] extends FAtomic[E] {

  type JsonType
  type DataType = E
  val writer: Encoder[JsonType]
  val convert: DataType => JsonType
  val typeTag: WeakTypeTag[JsonType]

}