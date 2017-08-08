package net.scalax.fsn.slick.atomic

import net.scalax.fsn.core.Atomic
import net.scalax.fsn.slick.helpers.FilterWrapper
import slick.lifted.{ FlatShapeLevel, Shape }

trait SlickDelete[E] extends Atomic[E] {

  type SourceType
  //type SlickType
  type TargetType
  type DataType = E
  //type FilterData

  val mainCol: SourceType
  val owner: Any
  val mainShape: Shape[_ <: FlatShapeLevel, SourceType, DataType, TargetType]
  val primaryGen: Option[FilterWrapper[TargetType, DataType]]
  /*val mainShape: Shape[_ <: FlatShapeLevel, SourceType, SlickType, TargetType]
  val primaryGen: Option[FilterWrapper[TargetType, FilterData]]
  val filterConvert: DataType => FilterData*/

}

case class SDelete[S, D, T](
    override val mainCol: S,
    override val owner: Any,
    override val mainShape: Shape[_ <: FlatShapeLevel, S, D, T],
    override val primaryGen: Option[FilterWrapper[T, D]] //,
//override val filterConvert: E => U
) extends SlickDelete[D] {
  override type SourceType = S
  //override type SlickType = D
  override type TargetType = T
  //override type FilterData = U
}