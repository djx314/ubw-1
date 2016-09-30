package net.scalax.fsn.slick_poi

import net.scalax.fsn.core._
import net.scalax.fsn.slick_common.{PropertyInfo, SlickMonad, SlickReader}
import org.xarcher.cpoi.CellData
import slick.lifted._

import scala.language.existentials

trait SlickPoiBind {
  def bind[E, U](query: Query[E, U, Seq]): Query[E, U, Seq]
  val target: List[SlickPoiConvert]
}

trait SlickPoiFShape extends FShape[SlickPoiBind, PoiQuery] with PoiMonad {

  import scalaz.Monoid

  override def encode(wQuery: SlickPoiBind): PoiQuery = {
    val poiMonad = implicitly[Monoid[SlickPoiConvert]]

    val fv = wQuery.target.foldLeft(poiMonad.zero)((s, t) => poiMonad.append(s, t))
    val fvQuery = Query(fv.font.sourceCol)(fv.font.effect)
    val mapQuery = wQuery.bind(fvQuery)
    val sortTargetGen: Map[String, fv.font.TargetColumn => ColumnOrdered[_]] = fv.font.orderTargetGen.map { case (key, value) =>
      key -> fv.font.orderGen.get(value).getOrElse(throw new Exception(s"$key 需要映射 $value 的排序方案，但找不到 $value 对应的列的排序"))
    } ++ fv.font.orderGen
    new PoiQuery {
      override type PoiE = fv.font.TargetColumn
      override type PoiU = fv.font.DataType
      override val uQuery = mapQuery
      override val sortMap = sortTargetGen
      override val properties = fv.propertyInfo
      override val render = (s: fv.font.DataType) => fv.back.effect(fv.convert(s))
    }
  }

}

trait PoiWriter extends FEffect {

  override type DataType
  override type Effect[T] = T => Map[String, CellData[_]]
  override val effect: Effect[DataType]

}

case class PWriter[T](override val effect: T => Map[String, CellData[_]]) extends PoiWriter {
  override type DataType = T
}

trait PoiMonad extends SlickMonad {

  import scalaz._

  type PoiWriterE[T] = FEffect.Aux[PoiWriter, T]

  implicit val poiWriterZip: Zip[PoiWriterE] = new Zip[PoiWriterE] {

    override def zip[A, B](f1: => PoiWriterE[A], f2: => PoiWriterE[B]): PoiWriterE[(A, B)] = {
      val (f1Case, f2Case) = (f1, f2)
      val newWriterDef: ((f1Case.DataType, f2Case.DataType)) => Map[String, CellData[_]] = s => {
        val (sourceData, appendData) = s
        f1Case.effect(sourceData) ++ f2.effect(appendData)
      }
      PWriter(newWriterDef)
    }

  }

  implicit val poiWriterZero: FEffectZero[PoiWriterE] = new FEffectZero[PoiWriterE] {

    def zero: PoiWriterE[Unit] = {
      PWriter((s: Unit) => Map())
    }

  }

  implicit val slickPoiConvertMonoid: Monoid[SlickPoiConvert] = new Monoid[SlickPoiConvert] {

    private val pWZip = implicitly[Zip[PoiWriterE]]
    private val pWZero = implicitly[FEffectZero[PoiWriterE]]
    private val sRZip = implicitly[Zip[SlickReaderE]]
    private val sRZero = implicitly[FEffectZero[SlickReaderE]]

    override def zero: SlickPoiConvert = {
      new SlickPoiConvert {
        override val propertyInfo = Nil
        override val font = sRZero.zero
        override val back = pWZero.zero
        override val convert = (s: Unit) => s
      }
    }
    override def append(f1: SlickPoiConvert, f2: => SlickPoiConvert): SlickPoiConvert = {
      val f2Case = f2
      val newReader = sRZip.zip(f1.font, f2Case.font)
      val newWriter = pWZip.zip(f1.back, f2Case.back)

      val newConvert = (s: newReader.DataType) => {
        val (sourceData, appendData) = s
        f1.convert(sourceData) -> f2Case.convert(appendData)
      }

      val newProInfo = f1.propertyInfo ::: f2Case.propertyInfo

      new SlickPoiConvert {
        override val propertyInfo = newProInfo
        override val font = newReader
        override val back = newWriter
        override val convert = newConvert
      }
    }
  }

}

trait SlickPoiConvert extends FEffectConverter {

  val propertyInfo: List[PropertyInfo]

  type Font = SlickReader
  type Back = PoiWriter

  val font: SlickReader

  val back: PoiWriter

  val convert: font.DataType => back.DataType

}