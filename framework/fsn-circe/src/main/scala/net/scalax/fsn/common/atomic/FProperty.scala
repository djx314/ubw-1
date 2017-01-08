package net.scalax.fsn.common.atomic

import net.scalax.fsn.core.FAtomic

trait FProperty[E] extends FAtomic[E] {
  val proName: String

  override def toString: String = s"FProperty(${ proName })"
}

trait FDescribe[E] extends FAtomic[E] {
  val describe: String

  override def toString: String = s"FDescribe(${ describe })"
}

trait DefaultValue[E] extends FAtomic[E] {
  val value: E

  override def toString: String = s"DefaultValue(${ value.toString })"
}