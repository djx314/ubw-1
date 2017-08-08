package net.scalax.fsn.mix.helpers

import net.scalax.fsn.core.Atomic
import net.scalax.fsn.slick.atomic._
import net.scalax.fsn.slick.helpers.{ FilterWrapper, SlickUtils }
import net.scalax.fsn.slick.model.StaticManyGen
import slick.lifted.{ FlatShapeLevel, Shape }

import scala.concurrent.Future

object In {

  def oneTOneR[S, D, T](sourceCol: S)(implicit shape: Shape[_ <: FlatShapeLevel, S, D, T], filterGen: FilterWrapper[T, D]): List[Atomic[D]] = List(new OneToOneRetrieve[D] {
    override type SourceType = S
    //override type SlickType = D
    override type TargetType = T
    //override type FilterData = D

    override val mainCol = sourceCol
    override val owner = SlickUtils.getTableIdFromCol(sourceCol)(shape)
    override val mainShape = shape
    override val primaryGen = filterGen
    //override val filterConvert = identity[D] _
  })

  def oneTOneU[S, D, T](sourceCol: S)(
    implicit
    shape: Shape[_ <: FlatShapeLevel, S, D, T],
    filterGen: FilterWrapper[T, D]
  ): List[Atomic[D]] = List(new OneToOneUpdate[D] {
    override type SourceType = S
    //override type SlickType = D
    override type TargetType = T
    //override type FilterData = D

    override val mainCol = sourceCol
    override val owner = SlickUtils.getTableIdFromCol(sourceCol)(shape)
    override val mainShape = shape
    override val primaryGen = filterGen
    //override val convert = identity[D] _
    //override val filterConvert = identity[D] _
  })

  def oneTOneC[S, D, T](sourceCol: S)(implicit shape: Shape[_ <: FlatShapeLevel, S, D, T]): List[Atomic[D]] = List(new OneToOneCrate[D] {
    override type SourceType = S
    //override type SlickType = D
    override type TargetType = T

    override val mainCol = sourceCol
    override val owner = SlickUtils.getTableIdFromCol(sourceCol)(shape)
    override val mainShape = shape
    //override val convert = identity[D] _
  })

  def oneTOne[S, D, T](sourceCol: S)(
    implicit
    shape: Shape[_ <: FlatShapeLevel, S, D, T],
    filterGen: FilterWrapper[T, D]
  ): List[Atomic[D]] = List(
    new OneToOneRetrieve[D] {
      override type SourceType = S
      //override type SlickType = D
      override type TargetType = T
      //override type FilterData = D

      override val mainCol = sourceCol
      override val owner = SlickUtils.getTableIdFromCol(sourceCol)(shape)
      override val mainShape = shape
      override val primaryGen = filterGen
      //override val filterConvert = identity[D] _
    },
    new OneToOneUpdate[D] {
      override type SourceType = S
      //override type SlickType = D
      override type TargetType = T
      //override type FilterData = D

      override val mainCol = sourceCol
      override val owner = SlickUtils.getTableIdFromCol(sourceCol)(shape)
      override val mainShape = shape
      override val primaryGen = filterGen
      //override val convert = identity[D] _
      //override val filterConvert = identity[D] _
    },
    new OneToOneCrate[D] {
      override type SourceType = S
      //override type SlickType = D
      override type TargetType = T

      override val mainCol = sourceCol
      override val owner = SlickUtils.getTableIdFromCol(sourceCol)(shape)
      override val mainShape = shape
      //override val convert = identity[D] _
    }
  )

  def staticMany[E](staticMany1: Future[List[StaticManyGen[E]]]): List[Atomic[E]] = List(new StaticMany[E] {
    val staticMany = staticMany1
  })

}