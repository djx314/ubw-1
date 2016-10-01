package net.scalax.fsn.slick.atomic

import aaaa.FilterWrapper1111
import bbbb.FRep
import net.scalax.fsn.core.FAtomic
import slick.lifted.{FlatShapeLevel, Shape}
import scala.language.existentials

trait SlickRetrieve[E] extends FAtomic[E] {

  type SourceType
  type SlickType
  type TargetType
  type DataType = E
  type FilterData

  val mainCol: FRep[SourceType]
  val mainShape: Shape[_ <: FlatShapeLevel, SourceType, SlickType, TargetType]
  val primaryGen: Option[FilterWrapper1111[TargetType, FilterData]]
  val convert: SlickType => DataType
  val filterConvert: DataType => FilterData

}

case class SRetrieve[S, D, T, A, E](
                                     override val mainCol: FRep[S],
                                     override val mainShape: Shape[_ <: FlatShapeLevel, S, D, T],
                                     override val primaryGen: Option[FilterWrapper1111[T, A]],
                                     override val convert: D => E,
                                     override val filterConvert: E => A
                                   ) extends SlickRetrieve[E] {
  override type SourceType = S
  override type SlickType = D
  override type TargetType = T
  override type FilterData = A
}