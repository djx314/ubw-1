package net.scalax.ubw.slick.atomic

import net.scalax.ubw.core.{ Atomic, AtomicPathImpl }
import net.scalax.ubw.slick.helpers.{ FilterWrapper, LikeableColumnGen }
import slick.lifted.{ ColumnOrdered, FlatShapeLevel, Shape }

trait StrSlickSelect[D] extends Atomic[D] {
  type SourceType
  type TargetType
  type DataType = D

  val shape: Shape[_ <: FlatShapeLevel, SourceType, DataType, TargetType]
  val outCol: SourceType
  val colToOrder: Option[TargetType => ColumnOrdered[_]]
  val filterGen: Option[FilterWrapper[TargetType, DataType]]
  val likeableGen: Option[LikeableColumnGen[TargetType]]

}

case class StrSSelect[S, D, T](
    override val shape: Shape[_ <: FlatShapeLevel, S, D, T],
    override val outCol: S,
    override val colToOrder: Option[T => ColumnOrdered[_]],
    override val filterGen: Option[FilterWrapper[T, D]],
    override val likeableGen: Option[LikeableColumnGen[T]]
) extends StrSlickSelect[D] with AtomicPathImpl[D] {
  override type SourceType = S
  override type TargetType = T
  override type DataType = D

  def filter(implicit priFilter: FilterWrapper[T, D]): StrSSelect[S, D, T] = {
    this.copy(
      filterGen = Option(priFilter)
    )
  }

  def likeable(implicit lickImplicit: LikeableColumnGen[T]): StrSSelect[S, D, T] = {
    this.copy(
      likeableGen = Option(lickImplicit)
    )
  }

  override val atomics = this :: Nil

  def order(implicit cv: T => ColumnOrdered[_]): StrSSelect[S, D, T] = {
    this.copy(colToOrder = Option(cv))
  }

  override def toString = s"StrSelect(canOrder=${colToOrder.isDefined})"
}

trait StrOrderNullsLast[E] extends Atomic[E] {
  val isOrderNullsLast: Boolean
}

trait StrDefaultDesc[E] extends Atomic[E] {
  val isDefaultDesc: Boolean
}

trait StrNeededFetch[E] extends Atomic[E] {
  val isInView: Boolean
}

trait StrOrderTargetName[E] extends Atomic[E] {
  val orderTargetName: String
}