package net.scalax.fsn.json.atomic

import cats.{ FlatMap, Functor }
import io.circe.Decoder
import net.scalax.fsn.core.Atomic
import net.scalax.fsn.slick.helpers.{ EqType, FilterModel }

trait JsonReader[E] extends Atomic[E] {

  type DataType = E

  val reader: Decoder[DataType]

}

object JsonReader {

  implicit val functorForOption: Functor[JsonReader] = new FlatMap[JsonReader] {
    override def map[A, B](fa: JsonReader[A])(f: A => B): JsonReader[B] = new JsonReader[B] {
      override val reader = {
        fa.reader.map(f)
      }
    }

    override def flatMap[A, B](fa: JsonReader[A])(f: A => JsonReader[B]): JsonReader[B] = new JsonReader[B] {
      override val reader = {
        fa.reader.flatMap(s => f(s).reader)
      }
    }

    override def tailRecM[A, B](init: A)(fn: A => JsonReader[Either[A, B]]): JsonReader[B] = {
      val eitherReader = fn(init)
      val decoder: Decoder[B] = Decoder.instance { s =>
        s.as(eitherReader.reader) match {
          case Left(err) =>
            Left(err)
          case Right(t) =>
            t match {
              case Left(aResult) =>
                s.as(tailRecM(aResult)(fn).reader) match {
                  case Left(err) =>
                    Left(err)
                  case Right(result) =>
                    Right(result)
                }
              case Right(result) =>
                Right(result)
            }
        }
      }
      new JsonReader[B] {
        override val reader = decoder
      }
    }
  }

}

trait SlickCompare[E] extends Atomic[E] {
  type DataType = E
  val reader: Decoder[FilterModel[E]]
}

trait SlickCompareData[E] extends Atomic[E] {
  val compare: FilterModel[E]
}

object SlickCompareData {
  implicit val functorForOption: Functor[SlickCompareData] = new Functor[SlickCompareData] {
    override def map[A, B](fa: SlickCompareData[A])(f: A => B): SlickCompareData[B] = new SlickCompareData[B] {
      override val compare = {
        FilterModel(fa.compare.like, fa.compare.eq.map(f), fa.compare.gt.map(f), fa.compare.lt.map(f))
      }
    }
  }
}

trait CompareToStringConvert[E] extends Atomic[E] {
  val strCv: EqType[E, String]
  val optStrCv: EqType[E, Option[String]]
}