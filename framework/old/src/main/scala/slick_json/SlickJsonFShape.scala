/*package net.scalax.fsn.slick_json

import io.circe.Json
import net.scalax.fsn.core._
import net.scalax.fsn.slick.model.SelectProperty
import net.scalax.fsn.slick_common.{SlickMonad, SlickReader}
import slick.lifted._

trait SlickJsonBind {
  def bind[E, U](query: Query[E, U, Seq]): Query[E, U, Seq]
  val target: List[SlickJsonConvert]
}

trait SlickJsonFShape extends FShape[SlickJsonBind, JsonQuery] with JsonMonad {

  import scalaz.Monoid

  override def encode(wQuery: SlickJsonBind): JsonQuery = {
    val slickJsonMono = implicitly[Monoid[SlickJsonConvert]]

    val fv = wQuery.target.foldLeft(slickJsonMono.zero)((s, t) => slickJsonMono.append(s, t))
    val fvQuery = Query(fv.font.sourceCol)(fv.font.effect)
    val mapQuery = wQuery.bind(fvQuery)
    val jsonSortTargetGen: Map[String, fv.font.TargetColumn => ColumnOrdered[_]] = fv.font.orderTargetGen.map { case (key, value) =>
      key -> fv.font.orderGen.get(value).getOrElse(throw new Exception(s"$key 需要映射 $value 的排序方案，但找不到 $value 对应的列的排序"))
    } ++ fv.font.orderGen
    new JsonQuery {
      override type JsonE = fv.font.TargetColumn
      override type JsonU = fv.font.DataType
      override val uQuery = mapQuery
      override val render = (s: fv.font.DataType) => fv.back.effect(fv.convert(s))
      override val properties = fv.propertyInfo
      override val sortMap = jsonSortTargetGen
    }
  }

}

trait JsonMonad extends SlickMonad {

  import scalaz._

  type JsonWriterE[T] = FEffect.Aux[JsonWriter, T]

  implicit val jsonWriterZip: Zip[JsonWriterE] = new Zip[JsonWriterE] {

    override def zip[A, B](f1: => JsonWriterE[A], f2: => JsonWriterE[B]): JsonWriterE[(A, B)] = {
      val (f1Case, f2Case) = (f1, f2)
      val newWriterDef: ((f1Case.DataType, f2Case.DataType)) => Map[String, Json] = s => {
        val (sourceData, appendData) = s
        f1Case.effect(sourceData) ++ f2Case.effect(appendData)
      }
      JWriter(newWriterDef)
    }

  }

  implicit val jsonWriterZero: FEffectZero[JsonWriterE] = new FEffectZero[JsonWriterE] {

    def zero: JsonWriterE[Unit] = JWriter(
      (s: Unit) => Map()
    )

  }

  implicit val slickJsonConvertMonoId: Monoid[SlickJsonConvert] = new Monoid[SlickJsonConvert] {

    private val jWZip = implicitly[Zip[JsonWriterE]]
    private val jWZero = implicitly[FEffectZero[JsonWriterE]]
    private val sRZip = implicitly[Zip[SlickReaderE]]
    private val sRZero = implicitly[FEffectZero[SlickReaderE]]
    //private val sRMonoId = implicitly[FEffectMonoId[SlickReader]]

    override def zero: SlickJsonConvert = new SlickJsonConvert {
      override val propertyInfo = Nil
      override val font = sRZero.zero
      override val back = jWZero.zero
      override val convert = (s: Unit) => s
    }

    override def append(f1: SlickJsonConvert, f2: => SlickJsonConvert): SlickJsonConvert = {
      val f2Case = f2

      val newReader = sRZip.zip(f1.font, f2Case.font)
      val newWriter = jWZip.zip(f1.back, f2Case.back)

      val newConvert: newReader.DataType => newWriter.DataType = s => {
        val (sourceData, appendData) = s
        f1.convert(sourceData) -> f2Case.convert(appendData)
      }

      val newProInfo = f1.propertyInfo ::: f2Case.propertyInfo

      new SlickJsonConvert {
        override val propertyInfo = newProInfo
        override val font = newReader
        override val back = newWriter
        override val convert = newConvert
      }
    }
  }

}

trait JsonWriter extends FEffect {

  override type DataType
  override type Effect[T] = T => Map[String, Json]
  override val effect: Effect[DataType]

}

case class JWriter[T](override val effect: T => Map[String, Json]) extends JsonWriter {

  override type DataType = T

}

trait SlickJsonConvert extends FEffectConverter {

  val propertyInfo: List[SelectProperty]

  override type Font = SlickReader
  override type Back = JsonWriter

  override val font: SlickReader
  override val back: JsonWriter

  override val convert: font.DataType => back.DataType

}*/