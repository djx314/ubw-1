package net.scalax.fsn.slick.atomic

import net.scalax.fsn.core.{ FAtomic, FAtomicPathImpl }
import slick.ast.BaseTypedType
import slick.lifted.{ FlatShapeLevel, Ordered, Rep, Shape }

trait GroupSlickSelect[D] extends FAtomic[D] {
  type SourceType
  type DataType
  type TargetType

  val shape: Shape[_ <: FlatShapeLevel, SourceType, DataType, TargetType]
  val outCol: SourceType
  val colToOrder: Option[TargetType => Ordered]
}

case class GroupSSelect[S, D, T](
    shape: Shape[_ <: FlatShapeLevel, S, D, T],
    outCol: S,
    colToOrder: Option[T => Ordered] = None
) extends FAtomicPathImpl[D] {
  self =>

  override val atomics = selectWithType[D] :: Nil

  def order(implicit orderCv: T => Ordered): GroupSSelect[S, D, T] = self.copy(colToOrder = Option(orderCv))

  private def selectWithType[E]: GroupSlickSelect[E] = new GroupSlickSelect[E] {
    override type SourceType = S
    override type DataType = D
    override type TargetType = T
    override val shape = self.shape //.asInstanceOf[Shape[_ <: FlatShapeLevel, Any, Any, Any]]
    override val outCol = self.outCol
    override val colToOrder = self.colToOrder //.asInstanceOf[Option[Any => ColumnOrdered[_]]]
  }

  /*def groupWithNonOpt[U](
    implicit
    colConvert: T => Rep[U],
    baseTypedType1: BaseTypedType[U]
  ): GroupByColumnWrap[Option[U]] = {
    val col = new GroupableNoOptionColumn[U] {
      override val targetColConvert = colConvert.asInstanceOf[Any => Rep[U]]
      override val baseTypedType = baseTypedType1
    }
    new GroupByColumnWrap[Option[U]] {
      override val atomics = col :: selectWithType[Option[U]] :: selectWithType[Option[U]] :: Nil
    }
  }*/

  def groupValue[U](implicit groupColAbs: GroupColImplicit[T, U]): GroupByColumnWrap[Option[U]] = {
    new GroupByColumnWrap[Option[U]] {
      override val atomics = groupColAbs.toGroupColumn :: selectWithType[Option[U]] :: Nil
    }
  }

  /*def groupWithOpt[U](
    implicit
    colConvert: T => Rep[Option[U]],
    baseTypedType1: BaseTypedType[U]
  ): GroupByColumnWrap[Option[U]] = {
    val col = new GroupableOptionColumn[U] {
      override val targetColConvert = colConvert.asInstanceOf[Any => Rep[Option[U]]]
      override val baseTypedType = baseTypedType1
    }
    new GroupByColumnWrap[Option[U]] {
      override val atomics = col :: selectWithType[Option[U]] :: Nil
    }
  }*/

  def countable: GroupByColumnWrap[Int] = {
    val col = new CountableGroupColumn[Nothing] {
    }

    new GroupByColumnWrap[Int] {
      override val atomics = col :: selectWithType[Int] :: Nil
    }
  }

}

object GroupColImplicit {
  implicit def aaaa[T](implicit by: BaseTypedType[T]): GroupColImplicit[Rep[T], T] = {
    new GroupColImplicit[Rep[T], T] {
      self =>
      val targetColConvert = identity[Rep[T]] _
      val baseTypedType = implicitly[BaseTypedType[T]]
      override def toGroupColumn: GroupableColumnBase[T] = {
        new GroupableNoOptionColumn[T] {
          override val targetColConvert = self.targetColConvert.asInstanceOf[Any => Rep[T]]
          override val baseTypedType = self.baseTypedType
        }
      }
    }
  }

  implicit def bbbb[T](implicit by: BaseTypedType[T]): GroupColImplicit[Rep[Option[T]], T] = {
    new GroupColImplicit[Rep[Option[T]], T] {
      self =>
      val targetColConvert = identity[Rep[Option[T]]] _
      val baseTypedType = implicitly[BaseTypedType[T]]
      override def toGroupColumn: GroupableColumnBase[T] = {
        new GroupableOptionColumn[T] {
          override val targetColConvert = self.targetColConvert.asInstanceOf[Any => Rep[Option[T]]]
          override val baseTypedType = self.baseTypedType
        }
      }
    }
  }
}

trait GroupColImplicit[T, U] {
  def toGroupColumn: GroupableColumnBase[U]
}

trait GroupByColumnWrap[T] extends FAtomicPathImpl[T]

sealed abstract trait GroupableColumnAbs[E] extends FAtomic[E] {
}

trait CountableGroupColumn[E] extends GroupableColumnAbs[Int] {
}

abstract trait GroupableColumnBase[E] extends GroupableColumnAbs[Option[E]] {
  val baseTypedType: BaseTypedType[E]
}

trait GroupableNoOptionColumn[E] extends GroupableColumnBase[E] {
  val targetColConvert: Any => Rep[E]
}

trait GroupableOptionColumn[E] extends GroupableColumnBase[E] {
  val targetColConvert: Any => Rep[Option[E]]
}