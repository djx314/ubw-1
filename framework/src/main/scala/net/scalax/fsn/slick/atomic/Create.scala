package net.scalax.fsn.slick.atomic

import net.scalax.fsn.core.FAtomic
import net.scalax.fsn.slick.helpers.FRep
import slick.lifted.{FlatShapeLevel, Shape}

import scala.language.existentials

trait SlickCreate[E] extends FAtomic[E] {

  type SourceType
  type SlickType
  type TargetType
  type DataType = E

  val mainCol: FRep[SourceType]
  val mainShape: Shape[_ <: FlatShapeLevel, SourceType, SlickType, TargetType]
  val convert: SlickType => DataType
  val reverseConvert: DataType => SlickType

}

case class SCreate[S, D, T, E](
                                override val mainCol: FRep[S],
                                override val mainShape: Shape[_ <: FlatShapeLevel, S, D, T],
                                override val convert: D => E,
                                override val reverseConvert: E => D
                              ) extends SlickCreate[E] {
  type SourceType = S
  type SlickType = D
  type TargetType = T
}

trait AutoInc[E] extends FAtomic[E] {
  val isAutoInc: Boolean
}