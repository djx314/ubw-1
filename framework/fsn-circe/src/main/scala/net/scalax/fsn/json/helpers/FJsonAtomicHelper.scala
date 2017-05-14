package net.scalax.fsn.slick.helpers

import io.circe.{ Decoder, Encoder }
import io.circe.generic.auto._
import net.scalax.fsn.json.atomic.{ JsonReader, JsonWriter, SlickCompare }
import net.scalax.fsn.json.operation.FAtomicHelper

import scala.reflect.runtime.universe._

trait FJsonAtomicHelper[E] extends FAtomicHelper[E] {

  def writeJ(implicit encoder: Encoder[E], tag: WeakTypeTag[E]) = path.appendAtomic(new JsonWriter[E] {
    override type JsonType = E
    override val writer = encoder
    override val convert = identity[E] _
    override val typeTag = tag
  })

  def readJ(implicit decoder: Decoder[E]) = path.appendAtomic(new JsonReader[E] {
    override type JsonType = E
    override val reader = decoder
    override val convert = identity[E] _
  })

  def readSlickComp(implicit decoder: Decoder[E], tag: WeakTypeTag[E]) = {
    path.appendAtomic(new SlickCompare[E] {
      override val reader = Decoder[FilterModel[E]]
    })
  }

}