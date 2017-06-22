package net.scalax.fsn.slick.helpers

import slick.ast._
import slick.lifted._

import scala.annotation.tailrec
import scala.reflect.ClassTag

object SlickUtils {

  /**
   * 搞了一晚上,算是大概知道 slick 的 shape 是做什么的了.
   * 以前一直认为 slick 的 shape 是负责渲染列的,
   * 其实渲染列的工作是 TypedType 做的.
   * shape 的工作只是把复杂结构的列组合(嵌套在 case class 里面, Tuple, HList 之类的 Rep)
   * 解析成 List[Rep],还有就是把求得的数据重新组装成对应的类 Tuple 结构.
   * 直接继承 Shape 的只有 2 个东西(不都是 class),一个是 object RepShape,另一个是 class ProductNodeShape,
   * 如果有第三个的话,我会直接找 @szeiger dalao 谈笑风生的,其他的 Shape 都是直接或间接继承这 2 个东西的,
   * 所以只要判断这 2 种情况就可以解构了,不过目前我只需要判断 Shape 拥有的列的长度,这就十分简单了
   */
  @tailrec
  def countColumns(columns: Seq[RepShape.type], shapes: Seq[Shape[_ <: ShapeLevel, _, _, _]]): Seq[RepShape.type] = {
    if (shapes.isEmpty) {
      columns
    } else {
      val repShapes = shapes.filter(s => s == RepShape).map(_ => RepShape)
      val hShapes = shapes.collect {
        case s: ProductNodeShape[ShapeLevel @unchecked, _, _, _, _] =>
          s.shapes
      }.flatten
      countColumns(repShapes ++: columns, hShapes)
    }
  }

  def shapeLength(shape: Shape[_ <: ShapeLevel, _, _, _]): Int = {
    countColumns(Nil, List(shape)).size
  }

  def isShapeEmpty(shape: Shape[_ <: ShapeLevel, _, _, _]): Boolean = {
    countColumns(Nil, List(shape)).isEmpty
  }
  /*def getTableIdFromRep(rep: Rep[_]): Any = {
    rep.toNode match {
      case Select(tableNode: TableNode, _) =>
        tableNode.profileTable
      case Select(pathNode, _) =>
        pathNode
    }
  }*/
  def getTableIdFromCol[S, D, T](rep: S)(implicit shape: Shape[_ <: ShapeLevel, S, D, T]): Any = {
    rep match {
      case table: AbstractTable[_] =>
        table.tableTag match {
          case r: RefTag => r.path
          case _ => table.tableNode.profileTable
        }
      case commonRep =>
        shape.toNode(commonRep) match {
          case Select(tableNode: TableNode, _) =>
            tableNode.profileTable
          case Select(pathNode, _) =>
            pathNode
          case r: Ref => r.pathString
        }
    }
  }

  def getTableIdFromTable(table: AbstractTable[_]): Any = {
    table.tableTag match {
      case r: RefTag => r.path
      case _ => table.tableNode.profileTable
    }
  }

}

final class ListAnyShape[Level <: ShapeLevel](override val shapes: Seq[Shape[_ <: ShapeLevel, _, _, _]])
    extends MappedProductShape[Level, Seq[Any], Seq[Any], Seq[Any], Seq[Any]] {
  override def getIterator(value: Seq[Any]): Iterator[Any] = value.toIterator
  override def getElement(value: Seq[Any], idx: Int): Any = value(idx)
  override def buildValue(elems: IndexedSeq[Any]): Any = elems
  override def copy(shapes: Seq[Shape[_ <: ShapeLevel, _, _, _]]): Shape[Level, _, _, _] = new ListAnyShape(shapes)
  override val classTag: ClassTag[Seq[Any]] = implicitly[ClassTag[Seq[Any]]]
}

final class ListColumnShape[Level <: ShapeLevel](override val shapes: Seq[Shape[_ <: ShapeLevel, _, _, _]])
    extends MappedProductShape[Level, List[Any], List[Any], List[Any], List[Any]] {

  override def getIterator(value: List[Any]): Iterator[Any] = {
    value.toIterator
  }
  override def getElement(value: List[Any], idx: Int): Any = {
    value(idx)
  }
  override def buildValue(elems: IndexedSeq[Any]): Any = {
    elems.toList
  }
  override def copy(shapes: Seq[Shape[_ <: ShapeLevel, _, _, _]]): Shape[Level, _, _, _] = new ListColumnShape(shapes)
  override val classTag: ClassTag[List[Any]] = implicitly[ClassTag[List[Any]]]

}