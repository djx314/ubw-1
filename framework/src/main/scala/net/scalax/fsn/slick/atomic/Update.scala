package net.scalax.fsn.slick.atomic

import net.scalax.fsn.core.FAtomic
import net.scalax.fsn.slick.helpers.{FRep, FilterWrapper}
import slick.lifted.{FlatShapeLevel, Shape}

import scala.language.existentials

trait SlickUpdate[E] extends FAtomic[E] {

  type SourceType
  type SlickType
  type TargetType
  type FilterData

  type DataType = E

  val mainCol: FRep[SourceType]
  val mainShape: Shape[_ <: FlatShapeLevel, SourceType, SlickType, TargetType]
  val primaryGen: Option[FilterWrapper[TargetType, FilterData]]
  val convert: DataType => SlickType
  val filterConvert: DataType => FilterData

}

case class SUpdate[S, D, T, C, E](
                                   override val mainCol: FRep[S],
                                   override val mainShape: Shape[_ <: FlatShapeLevel, S, D, T],
                                   override val primaryGen: Option[FilterWrapper[T, C]],
                                   override val convert: E => D,
                                   override val filterConvert: E => C
                                 ) extends SlickUpdate[E] {
  override type SourceType = S
  override type SlickType = D
  override type TargetType = T
  override type FilterData = C
}