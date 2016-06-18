package net.scalax.fsn.json_slick

import io.circe.{Encoder, Json}
import io.circe.syntax._
import net.scalax.fsn.core.{FEffConvert, FEffect, FEffectZero}

import scala.language.existentials
import slick.lifted._

import scala.concurrent.{ExecutionContext, Future}

trait PJsonWriter extends FEffect {

  override type DataType

  override type Effect[T] = T => Map[String, Json]
  override val effect: Effect[DataType]

}

case class PJWriter[T](override val effect: T => Map[String, Json]) extends PJsonWriter {

  override type DataType = T

}

trait PJsonReader extends FEffect {

  override type DataType

  override type Effect[T] = Map[String, Json] => T
  override val effect: Effect[DataType]

}

case class PJReader[T](override val effect: Map[String, Json] => T) extends PJsonReader {

  override type DataType = T

}

trait PSlickReader extends FEffect {

  type SourceColumn
  override type DataType
  type TargetColumn

  val sourceCol: SourceColumn

  override type Effect[T] = Shape[_ <: FlatShapeLevel, SourceColumn, T, TargetColumn]

  override val effect: Effect[DataType]

  val primaryGen: List[DataType => FilterWrapper[TargetColumn]]
  val staticManyMap: Future[Map[String, DataType => QueryJsonInfo]]

}

case class PSReader[S, D, T](
  override val sourceCol: S,
  override val effect: Shape[_ <: FlatShapeLevel, S, D, T],
  override val primaryGen: List[D => FilterWrapper[T]],
  override val staticManyMap: Future[Map[String, D => QueryJsonInfo]]
) extends PSlickReader {

  override type SourceColumn = S
  override type DataType = D
  override type TargetColumn = T

}

trait PJsonSlickConvert extends FEffConvert {

  val propertyInfo: List[PropertyInfo]
  val staticMany: Future[List[StaticManyUbw]]

  type Font = PJsonReader
  type Middle = PSlickReader
  type Back = PJsonWriter

  val font: PJsonReader
  val middle: PSlickReader
  val back: PJsonWriter

  val convertJtoS: font.DataType => middle.DataType
  val convertStoJ: middle.DataType => back.DataType

}

trait PJsonSlickMonad {

  import scalaz._

  type PJsonWriterE[T] = FEffect.Aux[PJsonWriter, T]

  implicit val pJsonWriterZero: FEffectZero[PJsonWriterE] = new FEffectZero[PJsonWriterE] {

    override def zero: PJsonWriterE[Unit] = PJWriter((s: Unit) => Map())

  }

  implicit val pJsonWriterZip: Zip[PJsonWriterE] = new Zip[PJsonWriterE] {

    override def zip[A, B](f1: => PJsonWriterE[A], f2: => PJsonWriterE[B]): PJsonWriterE[(A, B)] = {
      val (f1Case, f2Case) = (f1, f2)
      val newWriterDef: ((f1Case.DataType, f2Case.DataType)) => Map[String, Json] = s => {
        f1Case.effect(s._1) ++ f2Case.effect(s._2)
      }
      PJWriter(newWriterDef)
    }

  }

  type PJsonReaderE[T] = FEffect.Aux[PJsonReader, T]

  implicit val pJsonReaderZip: Zip[PJsonReaderE] = new Zip[PJsonReaderE] {

    override def zip[A, B](f1: => PJsonReaderE[A], f2: => PJsonReaderE[B]): PJsonReaderE[(A, B)] = {
      val (f1Case, f2Case) = (f1, f2)
      val newWriterDef: Map[String, Json] => (f1Case.DataType, f2Case.DataType) = s => {
        f1Case.effect(s) -> f2Case.effect(s)
      }
      PJReader(newWriterDef)
    }

  }

  implicit val pJsonReaderZero: FEffectZero[PJsonReaderE] = new FEffectZero[PJsonReaderE] {

    override def zero: PJsonReaderE[Unit] = PJReader(s => (()))

  }

  type PSlickReaderE[T] = FEffect.Aux[PSlickReader, T]

  implicit def pSlickReaderZip(implicit ec: ExecutionContext): Zip[PSlickReaderE] = new Zip[PSlickReaderE] {

    override def zip[A, B](f1: => PSlickReaderE[A], f2: => PSlickReaderE[B]): PSlickReaderE[(A, B)] = {
      val (f1Case, f2Case) = (f1, f2)
      val newShape = new TupleShape[
        FlatShapeLevel,
        (f1Case.SourceColumn, f2Case.SourceColumn),
        (f1Case.DataType, f2Case.DataType),
        (f1Case.TargetColumn, f2Case.TargetColumn)
        ](f1Case.effect, f2Case.effect)

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

      PSReader(f1Case.sourceCol -> f2Case.sourceCol, newShape, appendPrimaryGen, newFutureMany)
    }

  }

  implicit val pSlickReaderZero: FEffectZero[PSlickReaderE] = new FEffectZero[PSlickReaderE] {

    override def zero: PSlickReaderE[Unit] = PSReader(
      sourceCol = (()),
      effect = implicitly[Shape[FlatShapeLevel, Unit, Unit, Unit]],
      primaryGen = Nil,
      staticManyMap = Future successful Map()
    )

  }

  implicit def pJsonSlickConvertMonoId(implicit ec: ExecutionContext): Monoid[PJsonSlickConvert] = new Monoid[PJsonSlickConvert] {

    private val pSRZero = implicitly[FEffectZero[PSlickReaderE]]
    private val pSRZip = implicitly[Zip[PSlickReaderE]]
    private val pJRZero = implicitly[FEffectZero[PJsonReaderE]]
    private val pJRZip = implicitly[Zip[PJsonReaderE]]
    private val pJWZero = implicitly[FEffectZero[PJsonWriterE]]
    private val pJWZip = implicitly[Zip[PJsonWriterE]]


    override def zero: PJsonSlickConvert = {
      new PJsonSlickConvert {
        override val propertyInfo = Nil
        override val staticMany = Future successful Nil
        override val font = pJRZero.zero
        override val middle = pSRZero.zero
        override val back = pJWZero.zero
        override val convertJtoS = (s: Unit) => s
        override val convertStoJ = (s: Unit) => s
      }
    }

    override def append(f1: PJsonSlickConvert, f2: => PJsonSlickConvert): PJsonSlickConvert = {
      val f2Case = f2
      val newFont = pJRZip.zip(f1.font, f2Case.font)
      val newMiddle = pSRZip.zip(f1.middle, f2Case.middle)
      val newBack = pJWZip.zip(f1.back, f2Case.back)

      val newConvertJtoS = (s: newFont.DataType) => {
        val (sourceData, appendData) = s
        f1.convertJtoS(sourceData) -> f2Case.convertJtoS(appendData)
      }

      val newConvertStoJ = (s: newMiddle.DataType) => {
        val (sourceData, appendData) = s
        f1.convertStoJ(sourceData) -> f2Case.convertStoJ(appendData)
      }

      val newProInfo = f1.propertyInfo ::: f2Case.propertyInfo

      val newStaticMany = Future.sequence(f1.staticMany :: f2Case.staticMany :: Nil).map(_.flatten)

      new PJsonSlickConvert {
        override val propertyInfo = newProInfo
        override val staticMany = newStaticMany
        override val font = newFont
        override val middle = newMiddle
        override val back = newBack
        override val convertJtoS = newConvertJtoS
        override val convertStoJ = newConvertStoJ
      }
    }
  }

}