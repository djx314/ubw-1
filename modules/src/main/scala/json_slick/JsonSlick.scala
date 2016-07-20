package net.scalax.fsn.json_slick

import io.circe.{Encoder, Json}
import io.circe.syntax._
import net.scalax.fsn.core.{FEffect, FEffectConverter, FEffectZero}
import net.scalax.fsn.slick_common.JsonOut
import slick.dbio.DBIO

import scala.language.existentials
import slick.lifted._

import scala.concurrent.{ExecutionContext, Future}
import scala.reflect.runtime.universe._

trait JsonReader extends FEffect {

  override type DataType
  override type Effect[T] = Map[String, Json] => T
  override val effect: Effect[DataType]

}

case class JReader[T](override val effect: Map[String, Json] => T) extends JsonReader {

  override type DataType = T

}

trait JsonSlickMonad {

  import scalaz._

  type JsonReaderE[T] = FEffect.Aux[JsonReader, T]

  implicit val jsonReaderZip: Zip[JsonReaderE] = new Zip[JsonReaderE] {

    override def zip[A, B](f1: => JsonReaderE[A], f2: => JsonReaderE[B]): JsonReaderE[(A, B)] = {
      val (f1Case, f2Case) = (f1, f2)
      val newReaderDef: Map[String, Json] => ((f1Case.DataType, f2Case.DataType)) = s => {
        f1Case.effect(s) -> f2Case.effect(s)
      }
      JReader(newReaderDef)
    }

  }

  implicit val jsonReaderZero: FEffectZero[JsonReaderE] = new FEffectZero[JsonReaderE] {

    override def zero: JsonReaderE[Unit] = JReader(s => (()))

  }

  type SlickWriterE[T] = FEffect.Aux[SlickWriter, T]

  implicit def slickWriterZip(implicit ec: ExecutionContext): Zip[SlickWriterE] = new Zip[SlickWriterE] {

    override def zip[A, B](f1: => SlickWriterE[A], f2: => SlickWriterE[B]): SlickWriterE[(A, B)] = {
      val (f1Case, f2Case) = (f1, f2)

      val newShape = Shape.tuple2Shape(f1Case.effect, f2Case.effect)

      val appendPrimaryGen: List[((f1Case.DataType, f2Case.DataType)) => FilterWrapper[(f1Case.TargetColumn, f2Case.TargetColumn)]] = {
        type SlefTarget = f1Case.TargetColumn

        val thisWrappers = f1Case.primaryGen.map { s =>
          data: ((f1Case.DataType, f2Case.DataType)) => {
            val thisWrapper = s(data._1)
            new FilterWrapper[(SlefTarget, f2Case.TargetColumn)] {
              override type Target = thisWrapper.Target
              override val condition = thisWrapper.condition
              override val convert = (t: (SlefTarget, f2Case.TargetColumn)) => {
                thisWrapper.convert(t._1)
              }
            }
          }
        }

        val appendWrappers = f2Case.primaryGen.map { s =>
          data: ((f1Case.DataType, f2Case.DataType)) => {
            val appendWrapper = s(data._2)
            new FilterWrapper[(SlefTarget, f2Case.TargetColumn)] {
              override type Target = appendWrapper.Target
              override val condition = appendWrapper.condition
              override val convert = (t: (SlefTarget, f2Case.TargetColumn)) => {
                appendWrapper.convert(t._2)
              }
            }
          }
        }

        thisWrappers ::: appendWrappers

      }

      val newFutureMany = for {
        many <- f1Case.staticManyMap
        appendMany <- f2Case.staticManyMap
      } yield {
        val manyNew = for {
          (key, value) <- many
        } yield {
          val tran = (param: ((f1Case.DataType, f2Case.DataType))) => {
            value(param._1)
          }
          key -> tran
        }
        val appendNew = for {
          (key, value) <- appendMany
        } yield {
          val tran = (param: ((f1Case.DataType, f2Case.DataType))) => {
            value(param._2)
          }
          key -> tran
        }
        manyNew ++ appendNew
      }

      SWriter(f1Case.sourceCol -> f2Case.sourceCol, newShape, appendPrimaryGen, newFutureMany)
    }

  }

  implicit val slickWriterZero: FEffectZero[SlickWriterE] = new FEffectZero[SlickWriterE] {

    override def zero: SlickWriterE[Unit] = {
      SWriter(
        sourceCol = (()),
        effect = implicitly[Shape[FlatShapeLevel, Unit, Unit, Unit]],
        primaryGen = Nil,
        staticManyMap = Future successful Map()
      )
    }

  }

  implicit def slickJsonConvertMonoId(implicit ec: ExecutionContext): Monoid[JsonSlickConvert] = new Monoid[JsonSlickConvert] {

    private val jRZip = implicitly[Zip[JsonReaderE]]
    private val jRZero = implicitly[FEffectZero[JsonReaderE]]
    private val sWZip = implicitly[Zip[SlickWriterE]]
    private val sWZero = implicitly[FEffectZero[SlickWriterE]]

    override def zero: JsonSlickConvert = {
      val emptyFont = jRZero.zero
      val emptyBack = sWZero.zero
      val emptyConvert: emptyFont.DataType => emptyBack.DataType = identity

      new JsonSlickConvert {
        override val propertyInfo = Nil
        override val font = emptyFont
        override val back = emptyBack
        override val convert = emptyConvert
      }
    }

    override def append(f1: JsonSlickConvert, f2: => JsonSlickConvert): JsonSlickConvert = {
      val f2Case = f2
      val newReader = jRZip.zip(f1.font, f2Case.font)
      val newWriter = sWZip.zip(f1.back, f2Case.back)

      val newConvert = (s: newReader.DataType) => {
        val (sourceData, appendData) = s
        f1.convert(sourceData) -> f2Case.convert(appendData)
      }

      val newProInfo = f1.propertyInfo ::: f2Case.propertyInfo

      new JsonSlickConvert {
        override val propertyInfo = newProInfo
        override val font = newReader
        override val back = newWriter
        override val convert = newConvert
      }

    }
  }

  class SlickJsonInConvertMonoId(implicit execution: ExecutionContext) {

    private val jRZip = implicitly[Zip[JsonReaderE]]
    private val jRZero = implicitly[FEffectZero[JsonReaderE]]
    private val sWZip = implicitly[Zip[SlickWriterE]]
    private val sWZero = implicitly[FEffectZero[SlickWriterE]]

    def zero: JsonSlickInConvert = {
      val emptyFont = jRZero.zero
      val emptyFirst = sWZero.zero
      val emptySecond = sWZero.zero

      val emptyConvert: emptyFont.DataType => emptySecond.DataType = identity

      new JsonSlickInConvert {
        override val propertyInfo = Nil
        override val first = emptyFirst
        override val second = emptySecond
        override val font = emptyFont
        override val convert = emptyConvert
      }
    }

    def appendFirst(f1: JsonSlickInConvert, f2: => JsonSlickConvert): JsonSlickInConvert = {
      val f2Case = f2

      val newFirst = sWZip.zip(f1.first, f2Case.back)
      val newProInfo = f1.propertyInfo ::: f2Case.propertyInfo
      JsonSlickInConvertImpl(newProInfo, f1.font, newFirst, f1.second, f1.convert)

    }

    def appendSecond(f1: JsonSlickInConvert, f2: => JsonSlickConvert)(implicit ec: ExecutionContext): JsonSlickInConvert = {
      val f2Case = f2

      val newFont = jRZip.zip(f1.font, f2Case.font)
      val newSecond = sWZip.zip(f1.second, f2Case.back)
      val newProInfo = f1.propertyInfo ::: f2Case.propertyInfo
      JsonSlickInConvertImpl(
        newProInfo,
        newFont,
        f1.first,
        newSecond,
        { s: newFont.DataType =>
          val (fontThis, fontAppend) = s
          val secondThis = f1.convert(fontThis)
          val secondAppend = f2Case.convert(fontAppend)
          secondThis -> secondAppend
        }
      )

    }

  }

  def SlickJsonInConvertMonoId(implicit ec: ExecutionContext): SlickJsonInConvertMonoId = {

    new SlickJsonInConvertMonoId()(ec)

  }

}

trait FilterWrapper[E] {
  type Target <: Rep[_]
  val condition: CanBeQueryCondition[Target]
  val convert: E => Target

  def genFilter[U](query: Query[E, U, Seq]): Query[E, U, Seq] = {
    query.filter(data => convert(data))(condition)
  }
}

case class PropertyInfo(property: String, typeName: String, inRetrieve: Boolean, canOrder: Boolean, isDefaultDesc: Boolean, isAutoInc: Boolean, isPrimaryKey: Boolean = false)

case class StaticManyInfo(
  propertyInfo: List[PropertyInfo],
  model: Map[String, Json],
  many: Map[String, QueryJsonInfo]
)

case class UpdateStaticManyInfo(
  effectRows: Int,
  many: Map[String, QueryJsonInfo]
)

case class QueryJsonInfo(
  properties: List[PropertyInfo],
  jsonGen: JsonOut,
  retrieveGen: Map[String, Json] => DBIO[StaticManyInfo],
  insertGen: Map[String, Json] => DBIO[UpdateStaticManyInfo],
  deleteGen: Map[String, Json] => DBIO[Int],
  updateGen: Map[String, Json] => DBIO[UpdateStaticManyInfo],
  staticMany: Future[List[StaticManyUbw]]
)

case class StaticManyGen[T](
  //model 的属性名称
  proName: String,
  //关联表的主表 id 字段
  //masterIdField: String,
  //关联表的从表 id 字段
  slaveryIdField: String,
  gen: T => QueryJsonInfo,
  ubwGen: JsonOut
)

case class StaticManyUbw(
  //model 的属性名称
  proName: String,
  //关联表的主表 id 字段
  masterIdField: String,
  //关联表的从表 id 字段
  slaveryIdField: String,
  ubwGen: JsonOut
)

trait SlickWriter extends FEffect {

  type SourceColumn
  override type DataType
  type TargetColumn

  val sourceCol: SourceColumn

  override type Effect[T] = Shape[_ <: FlatShapeLevel, SourceColumn, T, TargetColumn]

  override val effect: Effect[DataType]

  val primaryGen: List[DataType => FilterWrapper[TargetColumn]]
  val staticManyMap: Future[Map[String, DataType => QueryJsonInfo]]

}

case class SWriter[S, D, T](
  override val sourceCol: S,
  override val effect: Shape[_ <: FlatShapeLevel, S, D, T],
  override val primaryGen: List[D => FilterWrapper[T]],
  override val staticManyMap: Future[Map[String, D => QueryJsonInfo]]
) extends SlickWriter {

  override type SourceColumn = S
  override type DataType = D
  override type TargetColumn = T

}

trait JsonSlickConvert extends FEffectConverter with JsonSlickMonad {

  val propertyInfo: List[PropertyInfo]

  type Font = JsonReader
  type Back = SlickWriter

  val font: JsonReader

  val back: SlickWriter

  val convert: font.DataType => back.DataType

}

trait JsonSlickInConvert {

  val propertyInfo: List[PropertyInfo]

  type Font = JsonReader
  type First = SlickWriter
  type Second = SlickWriter

  val font: JsonReader
  val first: SlickWriter
  val second: SlickWriter

  val convert: font.DataType => second.DataType

}

case class JsonSlickInConvertImpl[A, B, C](
  override val propertyInfo: List[PropertyInfo],
  override val font: FEffect.Aux[JsonReader, A],
  override val first: FEffect.Aux[SlickWriter, B],
  override val second: FEffect.Aux[SlickWriter, C],
  override val convert: A => C
) extends JsonSlickInConvert
