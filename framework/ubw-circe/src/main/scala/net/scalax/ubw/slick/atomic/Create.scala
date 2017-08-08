package net.scalax.fsn.slick.atomic

import net.scalax.fsn.core.Atomic
import slick.lifted.{ FlatShapeLevel, Shape }

trait SlickCreate[E] extends Atomic[E] {

  type SourceType
  //type SlickType
  type TargetType
  type DataType = E

  val mainCol: SourceType
  val owner: Any
  val mainShape: Shape[_ <: FlatShapeLevel, SourceType, DataType, TargetType]
  //val mainShape: Shape[_ <: FlatShapeLevel, SourceType, SlickType, TargetType]
  //val convert: SlickType => DataType
  //val reverseConvert: DataType => SlickType

}

case class SCreate[S, D, T](
    override val mainCol: S,
    override val owner: Any,
    override val mainShape: Shape[_ <: FlatShapeLevel, S, D, T] //,
//override val convert: D => E,
//override val reverseConvert: E => D
) extends SlickCreate[D] {
  type SourceType = S
  type SlickType = D
  type TargetType = T
}

trait AutoInc[E] extends Atomic[E] {
  val isAutoInc: Boolean
}