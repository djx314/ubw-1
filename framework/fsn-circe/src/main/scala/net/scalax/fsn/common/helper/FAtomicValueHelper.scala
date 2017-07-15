package net.scalax.fsn.json.operation

import net.scalax.fsn.common.atomic.{ DefaultValue, FFutureValue, FValue }
import net.scalax.fsn.core.FAtomicValueImpl

import scala.concurrent.Future
import scala.language.implicitConversions

trait FAtomicValueHelper {

  def emptyValue[D]: FAtomicValueImpl[D] = FAtomicValueImpl.apply(Option.empty)

  def set[D](value: D): FAtomicValueImpl[D] = FAtomicValueImpl.apply(Option {
    FValue.apply(value)
  })

  def setF[D](value: Future[D]): FAtomicValueImpl[D] = FAtomicValueImpl.apply(
    Option(FFutureValue.apply(value))
  )

  def setOpt[D](valueOpt: Option[D]): FAtomicValueImpl[D] =
    valueOpt match {
      case Some(s) =>
        FAtomicValueImpl.apply(Option {
          FValue.apply(s)
        })
      case _ =>
        emptyValue[D]
    }

  def mergeDefault[D](default: Option[DefaultValue[D]], atomicValue: FAtomicValueImpl[D]): Option[D] = {
    val defaultOpt = default.map(_.value)
    (defaultOpt -> atomicValue) match {
      case (_, FSomeValue(s)) => Option(s)
      case (defaultOpt @ Some(_), zero) =>
        defaultOpt
      case _ => Option.empty
    }
  }

}

object FSomeValue {

  def unapply[T](fValue: FAtomicValueImpl[T]): Option[T] = {
    fValue.atomics match {
      case Some(elem: FValue[T]) => Option(elem.value)
      case _ => Option.empty
    }
  }

}

object FFValue {

  def unapply[T](fValue: FAtomicValueImpl[T]): Option[Future[T]] = {
    fValue.atomics match {
      case Some(elem: FFutureValue[T]) => Option(elem.value)
      case _ => Option.empty
    }
  }

}