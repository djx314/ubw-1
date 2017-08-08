package net.scalax.fsn.slick.atomic

import net.scalax.fsn.core.Atomic
import net.scalax.fsn.slick.helpers.FilterWrapper
import net.scalax.fsn.slick.model.StaticManyGen
import slick.lifted.{ FlatShapeLevel, Shape }

import scala.concurrent.Future
/*trait SubUbw[E] extends Atomic[E] {
  val subCols: List[FColumn]
}*/
trait OneToOneRetrieve[E] extends Atomic[E] {
  type SourceType
  //type SlickType
  type TargetType
  //type FilterData
  type DataType = E

  val mainCol: SourceType
  val owner: Any
  //val mainShape: Shape[_ <: FlatShapeLevel, SourceType, SlickType, TargetType]
  val mainShape: Shape[_ <: FlatShapeLevel, SourceType, DataType, TargetType]
  val primaryGen: FilterWrapper[TargetType, DataType]
  //val filterConvert: DataType => FilterData
}

trait OneToOneCrate[E] extends Atomic[E] {
  type SourceType
  //type SlickType
  type TargetType
  type DataType = E

  val mainCol: SourceType
  val owner: Any
  //val mainShape: Shape[_ <: FlatShapeLevel, SourceType, SlickType, TargetType]
  val mainShape: Shape[_ <: FlatShapeLevel, SourceType, DataType, TargetType]
  //val convert: DataType => SlickType
}

trait OneToOneUpdate[E] extends Atomic[E] {
  type SourceType
  //type SlickType
  type TargetType
  //type FilterData
  type DataType = E

  val mainCol: SourceType
  val owner: Any
  //val mainShape: Shape[_ <: FlatShapeLevel, SourceType, SlickType, TargetType]
  val mainShape: Shape[_ <: FlatShapeLevel, SourceType, DataType, TargetType]
  val primaryGen: FilterWrapper[TargetType, DataType]
  //val convert: DataType => SlickType
  //val filterConvert: DataType => FilterData
}

trait StaticMany[E] extends Atomic[E] {

  type DataType = E

  val staticMany: Future[List[StaticManyGen[DataType]]]

}