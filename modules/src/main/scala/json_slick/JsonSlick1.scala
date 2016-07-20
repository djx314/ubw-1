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

trait SlickWriter extends FEffect {

  type IncSource
  type IncValue
  type InvTarget
  val incShape = Shape[_ <: FlatShapeLevel, IncSource, IncValue, InvTarget]

  type InsertSource
  type InsertValue
  type InsertTarget
  val insertShape = Shape[_ <: FlatShapeLevel, InsertSource, InsertValue, InsertTarget]

  override type DataType = (IncValue, InsertValue)

  val sourceCol: InsertSource

  override type Effect[T] = (IncValue, InsertValue) => DataType

  override val effect: Effect[DataType] = (s: (IncValue, InsertValue)) => s

}