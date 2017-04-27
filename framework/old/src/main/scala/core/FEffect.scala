/*package net.scalax.fsn.core

import scala.language.higherKinds

trait FEffect {

  type DataType

  type Effect[_]

  val effect: Effect[DataType]

}

object FEffect {
  type Aux[T <: FEffect, Out] = T { type DataType = Out }
}

trait FEffConvert

trait FEffectConverter extends FEffConvert {

  type Font <: FEffect
  type Back <: FEffect

  val font: Font
  val back: Back

  val convert: font.DataType => back.DataType

}*/