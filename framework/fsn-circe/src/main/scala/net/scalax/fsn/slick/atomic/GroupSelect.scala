package net.scalax.fsn.slick.atomic

import net.scalax.fsn.core.FAtomic
import slick.ast.{ BaseTypedType, TypedType }
import slick.lifted.{ ColumnOrdered, FlatShapeLevel, Rep, Shape }

import scala.language.existentials

trait GroupSlickSelect[D] extends FAtomic[D] {
  type SourceType
  type TargetType
  type DataType = D

  val shape: Shape[_ <: FlatShapeLevel, SourceType, DataType, TargetType]
  val outCol: SourceType
}

case class GroupSSelect[S, D, T](
    override val shape: Shape[_ <: FlatShapeLevel, S, D, T],
    override val outCol: S
) extends GroupSlickSelect[D] {
  self =>
  override type SourceType = S
  override type TargetType = T

  def groupWithNonOpt[U](
    implicit
    colConvert: T => Rep[U],
    baseTypedType1: BaseTypedType[U],
    typedType1: TypedType[Option[U]]
  ): GroupableNoOptionColumn[U] = {
    new GroupableNoOptionColumn[U] {
      override type SourceType = S
      override type TargetType = T
      override type DataType = D

      override val selectModel = self
      override val groupModel = implicitly[Shape[FlatShapeLevel, Rep[Option[U]], Option[U], Rep[Option[U]]]]
      override val targetColConvert = colConvert
      override val colToOrder = None
      override val baseTypedType = baseTypedType1
      override val typedType = typedType1
    }
  }

  def groupWithOpt[U](
    implicit
    colConvert: T => Rep[Option[U]],
    baseTypedType1: BaseTypedType[U],
    typedType1: TypedType[Option[U]]
  ): GroupableOptionColumn[U] = {
    new GroupableOptionColumn[U] {
      override type SourceType = S
      override type TargetType = T
      override type DataType = D

      override val selectModel = self
      override val groupModel = implicitly[Shape[FlatShapeLevel, Rep[Option[U]], Option[U], Rep[Option[U]]]]
      override val targetColConvert = colConvert
      override val colToOrder = None
      override val baseTypedType = baseTypedType1
      override val typedType = typedType1
    }
  }

  def countable: CountableGroupColumn[Nothing] = {
    new CountableGroupColumn[Nothing] {
      override type SourceType = S
      override type TargetType = T
      override type DataType = D

      override val selectModel = self
    }
  }

}

sealed trait GroupableColumnAbs[E] extends FAtomic[E] {
  type SourceType
  type TargetType
  type DataType

  val selectModel: GroupSSelect[SourceType, DataType, TargetType]
}

trait CountableGroupColumn[E] extends GroupableColumnAbs[Int]

abstract trait GroupableColumnBase[E] extends GroupableColumnAbs[Option[E]] {
  override type SourceType
  override type TargetType
  override type DataType
  type RepType = E

  override val selectModel: GroupSSelect[SourceType, DataType, TargetType]
  val groupModel: Shape[_ <: FlatShapeLevel, Rep[Option[E]], Option[E], Rep[Option[E]]]

  val colToOrder: Option[Rep[Option[E]] => ColumnOrdered[_]]
  val baseTypedType: BaseTypedType[E]
  val typedType: TypedType[Option[E]]
}

trait GroupableNoOptionColumn[E] extends GroupableColumnBase[E] {
  val targetColConvert: TargetType => Rep[E]
}

trait GroupableOptionColumn[E] extends GroupableColumnBase[E] {
  val targetColConvert: TargetType => Rep[Option[E]]
}