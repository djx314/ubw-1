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

trait CJsonReader extends FEffect {

  override type DataType
  override type Effect[T] = Map[String, Json] => T
  override val effect: Effect[DataType]

}

case class CJReader[T](override val effect: Map[String, Json] => T) extends JsonReader {

  override type DataType = T

}
/*sealed abstract trait CInsertWriter extends FEffect

trait CIncSlickWriter extends CInsertWriter {

  type IncSource
  type IncTarget
  val incCol: IncSource
  override type DataType

  override type Effect[T] = Shape[_ <: FlatShapeLevel, IncSource, T, IncTarget]
  override val effect: Shape[_ <: FlatShapeLevel, IncSource, DataType, IncTarget]

}

trait CInsertSlickWriter extends CInsertWriter {

  type InsertSource
  type InsertTarget
  val insertCol: InsertSource

  override type DataType
  override type Effect[T] = Shape[_ <: FlatShapeLevel, InsertSource, T, InsertTarget]
  override val effect: Shape[_ <: FlatShapeLevel, InsertSource, DataType, InsertTarget]

}*/
trait CSlickWriter extends FEffect {

  type IncSource
  type IncValue
  type IncTarget
  val incCol: IncSource
  val incShape: Shape[_ <: FlatShapeLevel, IncSource, IncValue, IncTarget]

  type InsertSource
  type InsertValue
  type InsertTarget
  val insertCol: InsertSource
  val insertShape: Shape[_ <: FlatShapeLevel, InsertSource, InsertValue, InsertTarget]

  override type DataType

  override type Effect[T] = (IncValue, InsertValue) => T

  override val effect: Effect[DataType]

}

trait CJsonSlickMonad {

  import scalaz._

  type CSlickWriterE[T] = FEffect.Aux[CSlickWriter, T]

  /*private def zipIncInc[T1, T2](f1: FEffect.Aux[CIncSlickWriter, T1], f2: FEffect.Aux[CIncSlickWriter, T2]): FEffect.Aux[CIncSlickWriter, (T1, T2)] = {
    new CIncSlickWriter {

      override type IncSource = (f1.IncSource, f2.IncSource)
      override type IncTarget = (f1.IncTarget, f2.IncTarget)
      override val incCol = f1.incCol -> f2.incCol

      override type DataType = (f1.DataType, f2.DataType)
      override val effect = new TupleShape[
        FlatShapeLevel,
        (f1.IncSource, f2.IncSource),
        (f1.DataType, f2.DataType),
        (f1.IncTarget, f2.IncTarget)
      ](f1.effect, f2.effect)

    }
  }

  private def zipInsertInsert[T1, T2](f1: FEffect.Aux[CInsertSlickWriter, T1], f2: FEffect.Aux[CInsertSlickWriter, T2]): FEffect.Aux[CInsertSlickWriter, (T1, T2)] = {
    new CInsertSlickWriter {

      override type InsertSource = (f1.InsertSource, f2.InsertSource)
      override type InsertTarget = (f1.InsertTarget, f2.InsertTarget)
      override val insertCol = f1.insertCol -> f2.insertCol

      override type DataType = (f1.DataType, f2.DataType)
      override val effect = new TupleShape[
        FlatShapeLevel,
        (f1.InsertSource, f2.InsertSource),
        (f1.DataType, f2.DataType),
        (f1.InsertTarget, f2.InsertTarget)
        ](f1.effect, f2.effect)

    }
  }*/

  private def zipWriteWrite[T1, T2](f1: FEffect.Aux[CSlickWriter, T1], f2: FEffect.Aux[CSlickWriter, T2]): FEffect.Aux[CSlickWriter, (T1, T2)] = {
    new CSlickWriter {
      override type IncSource = (f1.IncSource, f2.IncSource)
      override type IncValue = (f1.IncValue, f2.IncValue)
      override type IncTarget = (f1.IncTarget, f2.IncTarget)
      override val incCol = f1.incCol -> f2.incCol
      override val incShape = new TupleShape[
        FlatShapeLevel,
        (f1.IncSource, f2.IncSource),
        (f1.IncValue, f2.IncValue),
        (f1.IncTarget, f2.IncTarget)
      ]
      (f1.incShape, f2.incShape)

      override type InsertSource = (f1.InsertSource, f2.InsertSource)
      override type InsertValue = (f1.InsertValue, f2.InsertValue)
      override type InsertTarget = (f1.InsertTarget, f2.InsertTarget)
      override val insertCol = f1.insertCol -> f2.insertCol
      override val insertShape = new TupleShape[
        FlatShapeLevel,
        (f1.InsertSource, f2.InsertSource),
        (f1.InsertValue, f2.InsertValue),
        (f1.InsertTarget, f2.InsertTarget)
      ]
      (f1.insertShape, f2.insertShape)

      override type DataType = (f1.DataType, f2.DataType)

      override val effect = { (inc: IncValue, insert: InsertValue) =>
        f1.effect(inc._1, insert._1) -> f2.effect(inc._2, insert._2)
      }

    }
  }

  implicit def slickWriterZip(implicit ec: ExecutionContext): Zip[CSlickWriterE] = new Zip[CSlickWriterE] {

    override def zip[A, B](f1: => CSlickWriterE[A], f2: => CSlickWriterE[B]): CSlickWriterE[(A, B)] = {
      val (f1Case, f2Case) = f1 -> f2
      zipWriteWrite(f1Case, f2Case)
      /*f1Case -> f2Case match {
        case (f1Write: CIncSlickWriter, f2Write: CIncSlickWriter) if f1Write.isInstanceOf[CIncSlickWriter] && f2Write.isInstanceOf[CIncSlickWriter] =>
          zipIncInc(f1Write, f2Write)
        case (f1Write: CInsertSlickWriter, f2Write: CInsertSlickWriter) if f1Write.isInstanceOf[CInsertSlickWriter] && f2Write.isInstanceOf[CInsertSlickWriter] =>
          zipInsertInsert(f1Write, f2Write)
        case (f1Write: CSlickWriter, f2Write: CSlickWriter) if f1Write.isInstanceOf[CSlickWriter] && f2Write.isInstanceOf[CSlickWriter] =>
          zipWriteWrite(f1Write, f2Write)
      }*/
    }

  }

  implicit val slickWriterZero: FEffectZero[CSlickWriterE] = new FEffectZero[CSlickWriterE] {

    override def zero: CSlickWriterE[Unit] = {
      new CSlickWriter {
        override type IncSource = Unit
        override type IncValue = Unit
        override type IncTarget = Unit
        override val incCol = (())
        override val incShape = implicitly[Shape[FlatShapeLevel, Unit, Unit, Unit]]

        override type InsertSource = Unit
        override type InsertValue = Unit
        override type InsertTarget = Unit
        override val insertCol = (())
        override val insertShape = implicitly[Shape[FlatShapeLevel, Unit, Unit, Unit]]

        override type DataType = Unit

        override val effect = { (inc: IncValue, insert: InsertValue) =>
          (())
        }

      }
    }

  }

}