package net.scalax.fsn.json.operation

import net.scalax.fsn.common.atomic.{ DefaultValue, FFutureValue, FValue }
import net.scalax.fsn.core.FAtomicValueImpl

import scala.concurrent.Future
import scala.language.implicitConversions

trait FAtomicValueHelper {

  /*trait AtomicValueWrap[D] {
    atomicSelf =>
    val fAtomicValue: FAtomicValueImpl[D]

    def opt: Option[D] = fAtomicValue.atomics.flatMap { s =>
      s match {
        case valueWrap: FValue[D] => Option(valueWrap.value)
        case _ => None
      }
    }

    def isDefined: Boolean = fAtomicValue.atomics match {
      case Some(valueWrap: FValue[D]) => true
      case _ => false
    }

    def map[T](s: D => T): FAtomicValueImpl[T] = {
      setOpt(atomicSelf.opt.map(s))
    }

    def flatMap[T](s: D => FAtomicValueImpl[T]): FAtomicValueImpl[T] = {
      setOpt(atomicSelf.opt.flatMap(t => s(t).opt))
    }

    def get: D = opt.get
  }

  implicit def atomicValueWrapImplicit[D](atomicValue: FAtomicValueImpl[D]): AtomicValueWrap[D] = {
    new AtomicValueWrap[D] {
      override val fAtomicValue = atomicValue
    }
  }*/

  def emptyValue[D]: FAtomicValueImpl[D] = new FAtomicValueImpl[D] {
    override val atomics = None
  }

  def set[D](value: D): FAtomicValueImpl[D] = new FAtomicValueImpl[D] {
    val value1 = value
    override val atomics = Option {
      new FValue[D] {
        override val value = value1
      }
    }
  }

  def setF[D](value: Future[D]): FAtomicValueImpl[D] = new FAtomicValueImpl[D] {
    val value1 = value
    override val atomics = Option {
      new FFutureValue[D] {
        override val value = value1
      }
    }
  }

  def setOpt[D](valueOpt: Option[D]): FAtomicValueImpl[D] =
    valueOpt match {
      case Some(s) =>
        new FAtomicValueImpl[D] {
          override val atomics = Option {
            new FValue[D] {
              override val value = s
            }
          }
        }
      case _ =>
        emptyValue[D]
    }

  def mergeDefault[D](default: Option[DefaultValue[D]], atomicValue: FAtomicValueImpl[D]): Option[D] = {
    val defaultOpt = default.map(_.value)
    (defaultOpt -> atomicValue) match {
      case (_, FSomeValue(s)) => Option(s)
      case (defaultOpt @ Some(_), zero) =>
        defaultOpt
      case _ => None
    }
  }

}

object FSomeValue {

  def unapply[T](fValue: FAtomicValueImpl[T]): Option[T] = {
    fValue.atomics match {
      case Some(elem: FValue[T]) => Option(elem.value)
      case _ => None
    }
  }

}

object FFValue {

  def unapply[T](fValue: FAtomicValueImpl[T]): Option[Future[T]] = {
    fValue.atomics match {
      case Some(elem: FFutureValue[T]) => Option(elem.value)
      case _ => None
    }
  }

}