package net.scalax.ubw.slick.helpers

import net.scalax.ubw.json.operation.AtomicHelper
import net.scalax.ubw.slick.atomic._
import slick.lifted.{ FlatShapeLevel, Shape }
import scala.language.implicitConversions

trait FStrSelectExtAtomicHelper[E] extends AtomicHelper[E] {

  def hidden = path.appendAtomic(new StrNeededFetch[E] {
    override val isInView = false
  })

  def inView(inView: Boolean) = path.appendAtomic(new StrNeededFetch[E] {
    override val isInView = inView
  })

  def orderTarget(name: String) = path.appendAtomic(new StrOrderTargetName[E] {
    override val orderTargetName = name
  })

  def desc = path.appendAtomic(new StrDefaultDesc[E] {
    override val isDefaultDesc = true
  })

  def asc = path.appendAtomic(new StrDefaultDesc[E] {
    override val isDefaultDesc = false
  })

  def nullsLast = path.appendAtomic(new StrOrderNullsLast[E] {
    override val isOrderNullsLast = true
  })

  def nullsFirst = path.appendAtomic(new StrOrderNullsLast[E] {
    override val isOrderNullsLast = false
  })

}

trait StrFSSelectAtomicHelper {

  trait SSelectHelper[S, D, T] {
    val rep: S
    val shape: Shape[_ <: FlatShapeLevel, S, D, T]

    def out: StrSSelect[S, D, T] = StrSSelect(
      shape,
      rep,
      None,
      None,
      None
    )
  }

  implicit def slickOutHelper[S, D, T](rep1: S)(implicit shape1: Shape[_ <: FlatShapeLevel, S, D, T]): SSelectHelper[S, D, T] = {
    new SSelectHelper[S, D, T] {
      override val rep = rep1
      override val shape = shape1
    }
  }

}

trait GroupFSSelectAtomicHelper {

  trait GroupSSelectHelper[S, D, T] {
    val rep: S
    val shape: Shape[_ <: FlatShapeLevel, S, D, T]

    def groupOutput: GroupSSelect[S, D, T] = GroupSSelect(
      shape,
      rep
    )
  }

  implicit def slickGroupOutHelper[S, D, T](rep1: S)(implicit shape1: Shape[_ <: FlatShapeLevel, S, D, T]): GroupSSelectHelper[S, D, T] = {
    new GroupSSelectHelper[S, D, T] {
      override val rep = rep1
      override val shape = shape1
    }
  }

}
