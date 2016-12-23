package net.scalax.fsn.slick.helpers

import io.circe.Encoder
import net.scalax.fsn.json.atomic.JsonWriter
import net.scalax.fsn.json.operation.FAtomicHelper
import scala.reflect.runtime.universe._

trait FJsonAtomicHelper[E] extends FAtomicHelper[E] {

  def writeJ(implicit encoder: Encoder[E], tag: WeakTypeTag[E]) = append(new JsonWriter[E] {
    override type JsonType = E
    override val writer = encoder
    override val convert = identity[E] _
    override val typeTag = tag
  })

}