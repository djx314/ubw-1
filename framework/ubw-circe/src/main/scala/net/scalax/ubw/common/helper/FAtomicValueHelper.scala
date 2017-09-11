package net.scalax.ubw.json.operation

import net.scalax.ubw.common.atomic.{ DefaultValue, FFutureValue, FValue }
import net.scalax.ubw.core.AtomicValueImpl

import scala.concurrent.Future
import scala.language.implicitConversions

trait AtomicValueHelper {

  def emptyValue[D]: AtomicValueImpl[D] = AtomicValueImpl.apply(Option.empty)

  def set[D](value: D): AtomicValueImpl[D] = AtomicValueImpl.apply(Option {
    FValue.apply(value)
  })

  def setF[D](value: Future[D]): AtomicValueImpl[D] = AtomicValueImpl.apply(
    Option(FFutureValue.apply(value))
  )

  def setOpt[D](valueOpt: Option[D]): AtomicValueImpl[D] =
    valueOpt match {
      case Some(s) =>
        AtomicValueImpl.apply(Option {
          FValue.apply(s)
        })
      case _ =>
        emptyValue[D]
    }

  def mergeDefault[D](default: Option[DefaultValue[D]], atomicValue: AtomicValueImpl[D]): Option[D] = {
    val defaultOpt = default.map(_.value)
    (defaultOpt -> atomicValue) match {
      case (_, FSomeValue(s)) => Option(s)
      case (defaultOpt @ Some(_), AtomicValueImpl.Zero()) =>
        defaultOpt
      case _ => Option.empty
    }
  }

}

object FSomeValue {

  def unapply[T](fValue: AtomicValueImpl[T]): Option[T] = {
    fValue.atomics match {
      case Some(elem: FValue[T]) => Option(elem.value)
      case _ => Option.empty
    }
  }

}

object FFValue {

  def unapply[T](fValue: AtomicValueImpl[T]): Option[Future[T]] = {
    fValue.atomics match {
      case Some(elem: FFutureValue[T]) => Option(elem.value)
      case _ => Option.empty
    }
  }

}