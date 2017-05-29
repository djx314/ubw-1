package net.scalax.fsn.mix.helpers

import io.circe.{ Decoder, Encoder }
import net.scalax.fsn.common.atomic.{ DefaultValue, FProperty }
import net.scalax.fsn.core.FAtomic
import net.scalax.fsn.excel.atomic.{ PoiReader, PoiStyleTransform, PoiWriter }
import net.scalax.fsn.json.atomic.{ JsonReader, JsonWriter }
import net.scalax.fsn.slick.atomic._
import net.scalax.fsn.slick.helpers.{ FilterWrapper, SlickUtils }
import net.scalax.fsn.slick.model.StaticManyGen
import org.xarcher.cpoi.{ ReadableCellOperationAbs, StyleTransform, WriteableCellOperationAbs }
import slick.lifted.{ FlatShapeLevel, Shape }

import scala.concurrent.Future
import scala.reflect.runtime.universe._

object In {
  /*def subUbw[T](cols: FColumn*): List[FAtomic[T]] = new SubUbw[T] {
    override val subCols = cols.toList
  } :: Nil

  def subUbw[T](cols: List[FColumn]): List[FAtomic[T]] = new SubUbw[T] {
    override val subCols = cols
  } :: Nil*/
  def property[E](name: String) = new FProperty[E] {
    override val proName = name
  }

  def jRead[T](implicit decoder: Decoder[T]): List[FAtomic[T]] = List(new JsonReader[T] {
    override val reader = decoder
  })

  def default[T](data: T): List[FAtomic[T]] = List(new DefaultValue[T] {
    override val value = data
  })

  def jWrite[T](
    implicit
    encoder: Encoder[T]
  ): List[FAtomic[T]] = List(new JsonWriter[T] {
    override val writer = encoder
  })

  def create[S, D, T](sourceCol: S)(implicit shape: Shape[_ <: FlatShapeLevel, S, D, T]): List[FAtomic[D]] = List(new SlickCreate[D] {
    override type SourceType = S
    override type SlickType = D
    override type TargetType = T

    override val mainCol = sourceCol
    override val owner = SlickUtils.getTableIdFromCol(sourceCol)(shape)
    override val mainShape = shape
    override val convert = identity[D] _
    override val reverseConvert = identity[D] _
  })

  def autoInc[T]: List[FAtomic[T]] = List(new AutoInc[T] {
    override val isAutoInc = true
  })

  def oneTOneR[S, D, T](sourceCol: S)(implicit shape: Shape[_ <: FlatShapeLevel, S, D, T], filterGen: FilterWrapper[T, D]): List[FAtomic[D]] = List(new OneToOneRetrieve[D] {
    override type SourceType = S
    override type SlickType = D
    override type TargetType = T
    override type FilterData = D

    override val mainCol = sourceCol
    override val owner = SlickUtils.getTableIdFromCol(sourceCol)(shape)
    override val mainShape = shape
    override val primaryGen = filterGen
    override val filterConvert = identity[D] _
  })

  def oneTOneU[S, D, T](sourceCol: S)(
    implicit
    shape: Shape[_ <: FlatShapeLevel, S, D, T],
    filterGen: FilterWrapper[T, D]
  ): List[FAtomic[D]] = List(new OneToOneUpdate[D] {
    override type SourceType = S
    override type SlickType = D
    override type TargetType = T
    override type FilterData = D

    override val mainCol = sourceCol
    override val owner = SlickUtils.getTableIdFromCol(sourceCol)(shape)
    override val mainShape = shape
    override val primaryGen = filterGen
    override val convert = identity[D] _
    override val filterConvert = identity[D] _
  })

  def oneTOneC[S, D, T](sourceCol: S)(implicit shape: Shape[_ <: FlatShapeLevel, S, D, T]): List[FAtomic[D]] = List(new OneToOneCrate[D] {
    override type SourceType = S
    override type SlickType = D
    override type TargetType = T

    override val mainCol = sourceCol
    override val owner = SlickUtils.getTableIdFromCol(sourceCol)(shape)
    override val mainShape = shape
    override val convert = identity[D] _
  })

  def oneTOne[S, D, T](sourceCol: S)(
    implicit
    shape: Shape[_ <: FlatShapeLevel, S, D, T],
    filterGen: FilterWrapper[T, D]
  ): List[FAtomic[D]] = List(
    new OneToOneRetrieve[D] {
      override type SourceType = S
      override type SlickType = D
      override type TargetType = T
      override type FilterData = D

      override val mainCol = sourceCol
      override val owner = SlickUtils.getTableIdFromCol(sourceCol)(shape)
      override val mainShape = shape
      override val primaryGen = filterGen
      override val filterConvert = identity[D] _
    },
    new OneToOneUpdate[D] {
      override type SourceType = S
      override type SlickType = D
      override type TargetType = T
      override type FilterData = D

      override val mainCol = sourceCol
      override val owner = SlickUtils.getTableIdFromCol(sourceCol)(shape)
      override val mainShape = shape
      override val primaryGen = filterGen
      override val convert = identity[D] _
      override val filterConvert = identity[D] _
    },
    new OneToOneCrate[D] {
      override type SourceType = S
      override type SlickType = D
      override type TargetType = T

      override val mainCol = sourceCol
      override val owner = SlickUtils.getTableIdFromCol(sourceCol)(shape)
      override val mainShape = shape
      override val convert = identity[D] _
    }
  )

  def staticMany[E](staticMany1: Future[List[StaticManyGen[E]]]): List[FAtomic[E]] = List(new StaticMany[E] {
    val staticMany = staticMany1
  })

  def excelRead[E](implicit reader1: ReadableCellOperationAbs[E]): List[FAtomic[E]] = List(new PoiReader[E] {
    override type PoiType = E
    override val reader = reader1
    override val convert = identity[E] _
  })

  def excelWrite[E](trans: StyleTransform*)(implicit writer1: WriteableCellOperationAbs[E]): List[FAtomic[E]] = new PoiWriter[E] {
    override type PoiType = E
    override val writer = writer1
    override val convert = identity[E] _
  } :: new PoiStyleTransform[E] {
    override val transforms = trans.toList
  } :: Nil

  def excelWrite[E](implicit writer1: WriteableCellOperationAbs[E]): List[FAtomic[E]] = new PoiWriter[E] {
    override type PoiType = E
    override val writer = writer1
    override val convert = identity[E] _
  } :: Nil

}