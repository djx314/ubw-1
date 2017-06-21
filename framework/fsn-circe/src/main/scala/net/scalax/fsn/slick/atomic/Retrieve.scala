package net.scalax.fsn.slick.atomic

import net.scalax.fsn.core.FAtomic
import net.scalax.fsn.slick.helpers.FilterWrapper
import slick.lifted.{ FlatShapeLevel, Shape }

trait SlickRetrieve[E] extends FAtomic[E] {

  type SourceType
  type TargetType
  type DataType = E

  val mainCol: SourceType
  val owner: Any
  val mainShape: Shape[_ <: FlatShapeLevel, SourceType, DataType, TargetType]
  val primaryGen: Option[FilterWrapper[TargetType, DataType]]
  //val mainShape: Shape[_ <: FlatShapeLevel, SourceType, SlickType, TargetType]
  //val primaryGen: Option[FilterWrapper[TargetType, FilterData]]
  //val convert: SlickType => DataType
  //val filterConvert: DataType => FilterData

}

case class SRetrieve[S, D, T](
    override val mainCol: S,
    override val owner: Any,
    override val mainShape: Shape[_ <: FlatShapeLevel, S, D, T],
    override val primaryGen: Option[FilterWrapper[T, D]] //,
//override val convert: D => E,
//override val filterConvert: E => A
) extends SlickRetrieve[D] {
  override type SourceType = S
  //override type SlickType = D
  override type TargetType = T
  //override type FilterData = A
}