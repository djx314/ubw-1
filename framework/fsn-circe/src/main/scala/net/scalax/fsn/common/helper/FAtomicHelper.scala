package net.scalax.fsn.json.operation

import net.scalax.fsn.common.atomic.{DefaultValue, FDescribe, FProperty}
import net.scalax.fsn.core.FAtomic

trait FAtomicHelper[D] {

  val atomics: List[FAtomic[D]]

  def appendAll(atomics: List[FAtomic[D]]): List[FAtomic[D]] = this.atomics ::: atomics
  def append(atomic: FAtomic[D]): List[FAtomic[D]] = atomic :: this.atomics

}

trait FPropertyAtomicHelper[D] extends FAtomicHelper[D] {

  def named(name: String) = {
    append(new FProperty[D] {
      override val proName = name
    })
  }

  def describe(describe1: String) = {
    append(new FDescribe[D] {
      override val describe = describe1
    })
  }

}

trait FDefaultAtomicHelper[D] extends FAtomicHelper[D] {

  def defaultValue(default: D) = {
    append(new DefaultValue[D] {
      override val value = default
    })
  }

}