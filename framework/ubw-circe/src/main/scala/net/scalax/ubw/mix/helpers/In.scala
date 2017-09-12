package net.scalax.ubw.mix.helpers

import net.scalax.ubw.core.Atomic
import net.scalax.ubw.slick.atomic._
import net.scalax.ubw.slick.helpers.{ FilterWrapper, SlickUtils }
import slick.lifted.{ FlatShapeLevel, Shape }

object In {

  def oneTOneR[S, D, T](sourceCol: S)(implicit shape: Shape[_ <: FlatShapeLevel, S, D, T], filterGen: FilterWrapper[T, D]): List[Atomic[D]] = List(new OneToOneRetrieve[D] {
    override type SourceType = S
    override type TargetType = T

    override val mainCol = sourceCol
    override val owner = SlickUtils.getTableIdFromCol(sourceCol)(shape)
    override val mainShape = shape
    override val primaryGen = filterGen
  })

  def oneTOneU[S, D, T](sourceCol: S)(
    implicit
    shape: Shape[_ <: FlatShapeLevel, S, D, T],
    filterGen: FilterWrapper[T, D]
  ): List[Atomic[D]] = List(new OneToOneUpdate[D] {
    override type SourceType = S
    override type TargetType = T

    override val mainCol = sourceCol
    override val owner = SlickUtils.getTableIdFromCol(sourceCol)(shape)
    override val mainShape = shape
    override val primaryGen = filterGen
  })

  def oneTOneC[S, D, T](sourceCol: S)(implicit shape: Shape[_ <: FlatShapeLevel, S, D, T]): List[Atomic[D]] = List(new OneToOneCrate[D] {
    override type SourceType = S
    override type TargetType = T

    override val mainCol = sourceCol
    override val owner = SlickUtils.getTableIdFromCol(sourceCol)(shape)
    override val mainShape = shape
  })

  def oneTOne[S, D, T](sourceCol: S)(
    implicit
    shape: Shape[_ <: FlatShapeLevel, S, D, T],
    filterGen: FilterWrapper[T, D]
  ): List[Atomic[D]] = List(
    new OneToOneRetrieve[D] {
      override type SourceType = S
      override type TargetType = T

      override val mainCol = sourceCol
      override val owner = SlickUtils.getTableIdFromCol(sourceCol)(shape)
      override val mainShape = shape
      override val primaryGen = filterGen
    },
    new OneToOneUpdate[D] {
      override type SourceType = S
      override type TargetType = T

      override val mainCol = sourceCol
      override val owner = SlickUtils.getTableIdFromCol(sourceCol)(shape)
      override val mainShape = shape
      override val primaryGen = filterGen
    },
    new OneToOneCrate[D] {
      override type SourceType = S
      override type TargetType = T

      override val mainCol = sourceCol
      override val owner = SlickUtils.getTableIdFromCol(sourceCol)(shape)
      override val mainShape = shape
    }
  )
  /*def staticMany[E](staticMany1: Future[List[StaticManyGen[E]]]): List[Atomic[E]] = List(new StaticMany[E] {
    val staticMany = staticMany1
  })*/
}