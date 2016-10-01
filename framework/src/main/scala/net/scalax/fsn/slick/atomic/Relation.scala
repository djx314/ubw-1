package net.scalax.fsn.slick.atomic

import aaaa.FilterWrapper1111
import bbbb.FRep
import net.scalax.fsn.core.{FAtomic, FColumn}
import net.scalax.fsn.slick.model.StaticManyGen
import org.xarcher.cpoi.{ReadableCellOperationAbs, WriteableCellOperationAbs}
import slick.lifted.{FlatShapeLevel, Shape}

import scala.language.existentials
import scala.language.implicitConversions
import scala.concurrent.Future

trait SubUbw[E] extends FAtomic[E] {
  val subCols: List[FColumn]
}

trait OneToOneRetrieve[E] extends FAtomic[E] {

  type SourceType
  type SlickType
  type TargetType
  type FilterData
  type DataType = E

  val mainCol: FRep[SourceType]
  val mainShape: Shape[_ <: FlatShapeLevel, SourceType, SlickType, TargetType]
  val primaryGen: FilterWrapper1111[TargetType, FilterData]
  val filterConvert: DataType => FilterData

}

trait OneToOneCrate[E] extends FAtomic[E] {

  type SourceType
  type SlickType
  type TargetType
  type DataType = E

  val mainCol: FRep[SourceType]
  val mainShape: Shape[_ <: FlatShapeLevel, SourceType, SlickType, TargetType]
  val convert: DataType => SlickType

}

trait OneToOneUpdate[E] extends FAtomic[E] {

  type SourceType
  type SlickType
  type TargetType
  type FilterData
  type DataType = E

  val mainCol: FRep[SourceType]
  val mainShape: Shape[_ <: FlatShapeLevel, SourceType, SlickType, TargetType]
  val primaryGen: FilterWrapper1111[TargetType, FilterData]
  val convert: DataType => SlickType
  val filterConvert: DataType => FilterData

}

trait StaticMany[E] extends FAtomic[E] {

  type DataType = E

  val staticMany: Future[List[StaticManyGen[DataType]]]

}