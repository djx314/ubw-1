package net.scalax.fsn.slick.atomic

import net.scalax.fsn.core.{ FAtomic, FPathImpl }
import slick.ast.BaseTypedType
import slick.lifted.{ ColumnOrdered, FlatShapeLevel, Rep, Shape }

import scala.language.existentials

trait GroupSlickSelect[D] extends FAtomic[D] {
  val shape: Shape[_ <: FlatShapeLevel, Any, Any, Any]
  val outCol: Any
}

case class GroupSSelect[S, D, T](
    shape: Shape[_ <: FlatShapeLevel, S, D, T],
    outCol: S
) extends FPathImpl[D] {
  self =>

  override val atomics = selectWithType[D] :: Nil

  private def selectWithType[E]: GroupSlickSelect[E] = new GroupSlickSelect[E] {
    override val shape = self.shape.asInstanceOf[Shape[_ <: FlatShapeLevel, Any, Any, Any]]
    override val outCol = self.outCol
  }

  def groupWithNonOpt[U](
    implicit
    colConvert: T => Rep[U],
    baseTypedType1: BaseTypedType[U]
  ): GroupableNoOptionColumn[U] = {
    new GroupableNoOptionColumn[U] {
      override val targetColConvert = colConvert.asInstanceOf[Any => Rep[U]]
      override val colToOrder = None
      override val baseTypedType = baseTypedType1
      override val atomics = (this: GroupableNoOptionColumn[U]) :: selectWithType[Option[U]] :: Nil
    }
  }

  def groupWithOpt[U](
    implicit
    colConvert: T => Rep[Option[U]],
    baseTypedType1: BaseTypedType[U]
  ): GroupableOptionColumn[U] = {
    new GroupableOptionColumn[U] {
      override val targetColConvert = colConvert.asInstanceOf[Any => Rep[Option[U]]]
      override val colToOrder = None
      override val baseTypedType = baseTypedType1
      override val atomics = (this: GroupableOptionColumn[U]) :: selectWithType[Option[U]] :: Nil
    }
  }

  def countable: CountableGroupColumn[Nothing] = {
    new CountableGroupColumn[Nothing] {
      override val atomics = (this: CountableGroupColumn[Nothing]) :: selectWithType[Int] :: Nil
    }
  }

}

sealed trait GroupableColumnAbs[E] extends FAtomic[E] {
}

trait CountableGroupColumn[E] extends GroupableColumnAbs[Int] with FPathImpl[Int] {
}

abstract trait GroupableColumnBase[E] extends GroupableColumnAbs[Option[E]] {
  val colToOrder: Option[Rep[Option[E]] => ColumnOrdered[_]]
  val baseTypedType: BaseTypedType[E]
}

trait GroupableNoOptionColumn[E] extends GroupableColumnBase[E] with FPathImpl[Option[E]] {
  val targetColConvert: Any => Rep[E]
}

trait GroupableOptionColumn[E] extends GroupableColumnBase[E] with FPathImpl[Option[E]] {
  val targetColConvert: Any => Rep[Option[E]]
}