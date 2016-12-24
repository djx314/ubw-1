package net.scalax.fsn.slick.helpers

import net.scalax.fsn.json.operation.FAtomicHelper
import net.scalax.fsn.slick.atomic._
import slick.lifted.{FlatShapeLevel, Shape}
import scala.language.implicitConversions

trait FSSelectAtomicHelper {

  trait SSelectHelper[S, D, T, E] {
    val rep: S
    val shape: Shape[_ <: FlatShapeLevel, S, D, T]
    val convert: D => E

    def out: SSelect[S, D, T, E] = SSelect(
      shape,
      convert,
      rep,
      None
    )
  }

  implicit def slickOutHelper[S, D, T](rep1: S)(implicit shape1: Shape[_ <: FlatShapeLevel, S, D, T]): SSelectHelper[S, D, T, D] = {
    new SSelectHelper[S, D, T, D] {
      override val rep = rep1
      override val shape = shape1
      override val convert = identity[D] _
    }
  }

}

trait FSelectExtAtomicHelper[E] extends FAtomicHelper[E] {

  def hidden = append(new InRetrieve[E] {
    override val isInRetrieve = false
  })

  def orderTarget(name: String) = append(new OrderTargetName[E] {
    override val orderTargetName = name
  })

  def desc = append(new DefaultDesc[E] {
    override val isDefaultDesc = true
  })

  def asc = append(new DefaultDesc[E] {
    override val isDefaultDesc = false
  })

  def nullsLast = append(new OrderNullsLast[E] {
    override val isOrderNullsLast = true
  })

  def nullsFirst = append(new OrderNullsLast[E] {
    override val isOrderNullsLast = false
  })

}