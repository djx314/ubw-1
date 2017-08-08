package net.scalax.fsn.slick.helpers

import io.circe.{ Decoder, Encoder }
import io.circe.generic.auto._
import net.scalax.fsn.json.atomic.{ CompareToStringConvert, JsonReader, JsonWriter, SlickCompare }
import net.scalax.fsn.json.operation.AtomicHelper

trait FJsonAtomicHelper[E] extends AtomicHelper[E] {

  def writeJ(implicit encoder: Encoder[E]) = path.appendAtomic(new JsonWriter[E] {
    override val writer = encoder
  })

  def readJ(implicit decoder: Decoder[E]) = path.appendAtomic(new JsonReader[E] {
    override val reader = decoder
  })

  def readSlickComp(implicit decoder: Decoder[E], strCv1: EqType[E, String] = null, optStrCv1: EqType[E, Option[String]] = null) = {
    path.appendAllAtomic(
      new SlickCompare[E] {
        override val reader = Decoder[FilterModel[E]]
      },
      new CompareToStringConvert[E] {
        override val strCv = strCv1
        override val optStrCv = optStrCv1
      }
    )
  }

}