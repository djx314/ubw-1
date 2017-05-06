package net.scalax.fsn.slick.atomic

import net.scalax.fsn.core.{ FAtomic, FAtomicPathImpl }
import slick.lifted.{ ColumnOrdered, FlatShapeLevel, Shape }

trait SlickSelect[E] extends FAtomic[E] {
  type SourceType
  type SlickType
  type TargetType
  type DataType = E

  val shape: Shape[_ <: FlatShapeLevel, SourceType, SlickType, TargetType]
  val outConvert: SlickType => DataType
  val outCol: SourceType
  val colToOrder: Option[TargetType => ColumnOrdered[_]]
}

case class SSelect[S, D, T, E](
    override val shape: Shape[_ <: FlatShapeLevel, S, D, T],
    override val outConvert: D => E,
    override val outCol: S,
    override val colToOrder: Option[T => ColumnOrdered[_]]
) extends SlickSelect[E] with FAtomicPathImpl[E] {
  override type SourceType = S
  override type SlickType = D
  override type TargetType = T
  override type DataType = E

  def order(implicit cv: T => ColumnOrdered[_]): SSelect[S, D, T, E] = {
    this.copy(colToOrder = Option(cv))
  }

  override val atomics: List[FAtomic[E]] = this :: Nil
}

trait OrderNullsLast[E] extends FAtomic[E] {
  val isOrderNullsLast: Boolean
}

trait DefaultDesc[E] extends FAtomic[E] {
  val isDefaultDesc: Boolean
}

trait InRetrieve[E] extends FAtomic[E] {
  val isInRetrieve: Boolean
}

trait OrderTargetName[E] extends FAtomic[E] {
  val orderTargetName: String
}