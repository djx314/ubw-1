package net.scalax.fsn.json.operation

import net.scalax.fsn.common.atomic.{ DefaultValue, FDescribe, FProperty }
import net.scalax.fsn.core.AtomicPathImpl

trait AtomicHelper[D] {

  val path: AtomicPathImpl[D]

  /*def appendAll(atomics: List[Atomic[D]]): List[Atomic[D]] = this.atomics ::: atomics
  def append(atomic: Atomic[D]): List[Atomic[D]] = atomic :: this.atomics*/

}

trait FPropertyAtomicHelper[D] extends AtomicHelper[D] {

  def named(name: String) = {
    path.appendAtomic(FProperty.apply(name))
  }

  def describe(describe1: String) = {
    path.appendAtomic(FDescribe.apply(describe1))
  }

}

trait FDefaultAtomicHelper[D] extends AtomicHelper[D] {

  def defaultValue(default: D) = {
    path.appendAtomic(DefaultValue.apply(default))
  }

}