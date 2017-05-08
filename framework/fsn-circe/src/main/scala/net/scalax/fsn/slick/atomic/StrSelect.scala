package net.scalax.fsn.slick.atomic

import net.scalax.fsn.core.{ FAtomic, FAtomicPathImpl }
import slick.lifted.{ ColumnOrdered, FlatShapeLevel, Shape }

trait StrSlickSelect[D] extends FAtomic[D] {
  type SourceType
  type TargetType
  type DataType = D

  val shape: Shape[_ <: FlatShapeLevel, SourceType, DataType, TargetType]
  val outCol: SourceType
  val colToOrder: Option[TargetType => ColumnOrdered[_]]
}

case class StrSSelect[S, D, T](
    override val shape: Shape[_ <: FlatShapeLevel, S, D, T],
    override val outCol: S,
    override val colToOrder: Option[T => ColumnOrdered[_]]
) extends StrSlickSelect[D] with FAtomicPathImpl[D] {
  override type SourceType = S
  override type TargetType = T
  override type DataType = D

  override val atomics = this :: Nil

  def order(implicit cv: T => ColumnOrdered[_]): StrSSelect[S, D, T] = {
    this.copy(colToOrder = Option(cv))
  }

  override def toString = s"StrSelect(canOrder=${colToOrder.isDefined})"
}

trait StrOrderNullsLast[E] extends FAtomic[E] {
  val isOrderNullsLast: Boolean
}

trait StrDefaultDesc[E] extends FAtomic[E] {
  val isDefaultDesc: Boolean
}

trait StrNeededFetch[E] extends FAtomic[E] {
  val isInView: Boolean
}

trait StrOrderTargetName[E] extends FAtomic[E] {
  val orderTargetName: String
}