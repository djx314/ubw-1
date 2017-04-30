package net.scalax.fsn.slick.atomic

import net.scalax.fsn.core.FAtomic
import net.scalax.fsn.slick.helpers.FilterWrapper
import net.scalax.fsn.slick.model.StaticManyGen
import slick.lifted.{ FlatShapeLevel, Shape }

import scala.concurrent.Future
/*trait SubUbw[E] extends FAtomic[E] {
  val subCols: List[FColumn]
}*/
trait OneToOneRetrieve[E] extends FAtomic[E] {

  type SourceType
  type SlickType
  type TargetType
  type FilterData
  type DataType = E

  val mainCol: SourceType
  val owner: Any
  val mainShape: Shape[_ <: FlatShapeLevel, SourceType, SlickType, TargetType]
  val primaryGen: FilterWrapper[TargetType, FilterData]
  val filterConvert: DataType => FilterData

}

trait OneToOneCrate[E] extends FAtomic[E] {

  type SourceType
  type SlickType
  type TargetType
  type DataType = E

  val mainCol: SourceType
  val owner: Any
  val mainShape: Shape[_ <: FlatShapeLevel, SourceType, SlickType, TargetType]
  val convert: DataType => SlickType

}

trait OneToOneUpdate[E] extends FAtomic[E] {

  type SourceType
  type SlickType
  type TargetType
  type FilterData
  type DataType = E

  val mainCol: SourceType
  val owner: Any
  val mainShape: Shape[_ <: FlatShapeLevel, SourceType, SlickType, TargetType]
  val primaryGen: FilterWrapper[TargetType, FilterData]
  val convert: DataType => SlickType
  val filterConvert: DataType => FilterData

}

trait StaticMany[E] extends FAtomic[E] {

  type DataType = E

  val staticMany: Future[List[StaticManyGen[DataType]]]

}