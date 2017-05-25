package net.scalax.fsn.slick.helpers

import net.scalax.fsn.core.FAtomicValueImpl
import net.scalax.fsn.json.atomic.SlickCompareData

import scala.language.implicitConversions

trait EqType[F, V] {
  def to(in: F): V
  def from(out: V): F
}

object EqType {
  implicit def eqTypeImplicit[F, V](implicit inCv: F <:< V, outCv: V <:< F): EqType[F, V] = new EqType[F, V] {
    override def to(in: F): V = inCv(in)
    override def from(out: V): F = outCv(out)
  }
}

case class FilterModel[T](
  like: Option[String] = None,
  eq: Option[T] = None,
  gt: Option[T] = None,
  lt: Option[T] = None
)

trait FilterModelHelper {

  trait AtomicFilterWrap[D] {
    atomicSelf =>
    val fAtomicValue: FAtomicValueImpl[D]

    def opt: Option[FilterModel[D]] = fAtomicValue.atomics.flatMap { s =>
      s match {
        case valueWrap: SlickCompareData[D] => Option(valueWrap.compare)
        case _ => None
      }
    }

    def isDefined: Boolean = fAtomicValue.atomics match {
      case Some(valueWrap: SlickCompareData[D]) => true
      case _ => false
    }

    def map[T](cv: D => T): FAtomicValueImpl[T] = {
      setOpt(atomicSelf.opt.map { s =>
        FilterModel(s.like, s.eq.map(cv), s.gt.map(cv), s.lt.map(cv))
      })
    }

    def flatMap[T](s: FilterModel[D] => FAtomicValueImpl[T]): FAtomicValueImpl[T] = {
      setOpt(atomicSelf.opt.flatMap(t => s(t).opt))
    }

    def get: FilterModel[D] = opt.get
  }

  implicit def atomicValueWrapImplicit[D](atomicValue: FAtomicValueImpl[D]): AtomicFilterWrap[D] = {
    new AtomicFilterWrap[D] {
      override val fAtomicValue = atomicValue
    }
  }

  def emptyValue[D]: FAtomicValueImpl[D] = new FAtomicValueImpl[D] {
    override val atomics = None
  }

  def set[D](value: FilterModel[D]): FAtomicValueImpl[D] = new FAtomicValueImpl[D] {
    val value1 = value
    override val atomics = Option {
      new SlickCompareData[D] {
        override val compare = value1
      }
    }
  }

  def setOpt[D](valueOpt: Option[FilterModel[D]]): FAtomicValueImpl[D] =
    valueOpt match {
      case Some(s) =>
        new FAtomicValueImpl[D] {
          override val atomics = Option {
            new SlickCompareData[D] {
              override val compare = s
            }
          }
        }
      case _ =>
        emptyValue[D]
    }

}