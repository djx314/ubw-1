package net.scalax.fsn.slick.atomic

import aaaa.FilterWrapper1111
import bbbb.FRep
import net.scalax.fsn.core.FAtomic
import slick.lifted.{FlatShapeLevel, Shape}
import scala.language.existentials

trait SlickDelete[E] extends FAtomic[E] {

  type SourceType
  type SlickType
  type TargetType
  type DataType = E
  type FilterData

  val mainCol: FRep[SourceType]
  val mainShape: Shape[_ <: FlatShapeLevel, SourceType, SlickType, TargetType]
  val primaryGen: Option[FilterWrapper1111[TargetType, FilterData]]
  val filterConvert: DataType => FilterData

}

case class SDelete[S, D, T, U, E](
                                   override val mainCol: FRep[S],
                                   override val mainShape: Shape[_ <: FlatShapeLevel, S, D, T],
                                   override val primaryGen: Option[FilterWrapper1111[T, U]],
                                   override val filterConvert: E => U
                                 ) extends SlickDelete[E] {
  override type SourceType = S
  override type SlickType = D
  override type TargetType = T
  override type FilterData = U
}