package net.scalax.fsn.json.operation

import net.scalax.fsn.common.atomic.{ DefaultValue, FDescribe, FProperty, FValue }
import net.scalax.fsn.core.{ FAtomicPathImpl, FAtomicValueImpl }

trait FAtomicValueHelper {

  trait AtomicValueWrap[D] {
    val fAtomicValue: FAtomicValueImpl[D]

    def opt: Option[D] = fAtomicValue.atomic.flatMap { s =>
      s match {
        case valueWrap: FValue[D] => Option(valueWrap.value)
        case _ => None
      }
    }

    def get: D = opt.get
  }

  implicit def atomicValueWrapImplicit[D](atomicValue: FAtomicValueImpl[D]): AtomicValueWrap[D] = {
    new AtomicValueWrap[D] {
      override val fAtomicValue = atomicValue
    }
  }

  def set[D](value: D): AtomicValueWrap[D] = new FAtomicValueImpl[D] {
    val value1 = value
    override val atomic = Option {
      new FValue[D] {
        override val value = value1
      }

    }

  }

  def mergeDefault[D](default: Option[DefaultValue[D]], atomicValue: FAtomicValueImpl[D]): Option[D] = {
    val defaultOpt = default.map(_.value)
    (defaultOpt -> atomicValue.opt) match {
      case (_, valueOpt @ Some(_)) => valueOpt
      case (defaultOpt @ Some(_), None) => defaultOpt
      case _ => None
    }
  }

}