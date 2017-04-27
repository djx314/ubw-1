/*package net.scalax.fsn.slick_common

import net.scalax.fsn.core.{FEffect, FEffectZero}
import slick.lifted._

import scalaz._
import scala.language.existentials

trait SlickMonad {

  type SlickReaderE[T] = FEffect.Aux[SlickReader, T]

  implicit val slickReaderZip: Zip[SlickReaderE] = new Zip[SlickReaderE] {

    override def zip[A, B](f1: => SlickReaderE[A], f2: => SlickReaderE[B]): SlickReaderE[(A, B)] = {
      val (f1Case, f2Case) = (f1, f2)
      val newShape = Shape.tuple2Shape(f1Case.effect, f2Case.effect)

      val sourceOrderGen: Map[String, ((f1Case.TargetColumn, f2Case.TargetColumn)) => ColumnOrdered[_]] =
        for {
          (key, gen) <- f1Case.orderGen
        } yield {
          val newGen = (s: (f1Case.TargetColumn, f2Case.TargetColumn)) => {
            val (sTargetCol, _) = s
            gen(sTargetCol)
          }
          key -> newGen
        }

      val appendOrderGen: Map[String, ((f1Case.TargetColumn, f2Case.TargetColumn)) => ColumnOrdered[_]] =
        for {
          (key, gen) <- f2Case.orderGen
        } yield {
          val newGen = (s: (f1Case.TargetColumn, f2Case.TargetColumn)) => {
            val (_, aTargetCol) = s
            gen(aTargetCol)
          }
          key -> newGen
        }

      SReader(f1Case.sourceCol -> f2Case.sourceCol, newShape, sourceOrderGen ++ appendOrderGen, f1Case.orderTargetGen ++ f2Case.orderTargetGen)
    }

  }

  implicit val slickReaderZero: FEffectZero[SlickReaderE] = new FEffectZero[SlickReaderE] {

    override def zero: SlickReaderE[Unit] = SReader(
      (()),
      implicitly[Shape[FlatShapeLevel, Unit, Unit, Unit]],
      Map(),
      Map()
    )

  }

}

trait SlickReader extends FEffect {

  type SourceColumn
  override type DataType
  type TargetColumn

  val orderGen: Map[String, TargetColumn => ColumnOrdered[_]]
  val orderTargetGen: Map[String, String]
  val sourceCol: SourceColumn

  override type Effect[T] = Shape[_ <: FlatShapeLevel, SourceColumn, T, TargetColumn]

  override val effect: Effect[DataType]

}

case class SReader[S, D, T](
  override val sourceCol: S,
  override val effect: Shape[_ <: FlatShapeLevel, S, D, T],
  override val orderGen: Map[String, T => ColumnOrdered[_]],
  override val orderTargetGen: Map[String, String]
) extends SlickReader {

  override type SourceColumn = S
  override type DataType = D
  override type TargetColumn = T

}*/