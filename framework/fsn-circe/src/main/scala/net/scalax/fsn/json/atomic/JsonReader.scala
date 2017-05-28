package net.scalax.fsn.json.atomic

import cats.Functor
import io.circe.Decoder
import net.scalax.fsn.core.FAtomic
import net.scalax.fsn.slick.helpers.{ EqType, FilterModel }

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

object SlickCompareData {
  implicit val functorForOption: Functor[SlickCompareData] = new Functor[SlickCompareData] {
    def map[A, B](fa: SlickCompareData[A])(f: A => B): SlickCompareData[B] = new SlickCompareData[B] {
      override val compare = {
        FilterModel(fa.compare.like, fa.compare.eq.map(f), fa.compare.gt.map(f), fa.compare.lt.map(f))
      }
    }
  }
}

trait CompareToStringConvert[E] extends FAtomic[E] {
  val strCv: EqType[E, String]
  val optStrCv: EqType[E, Option[String]]
}