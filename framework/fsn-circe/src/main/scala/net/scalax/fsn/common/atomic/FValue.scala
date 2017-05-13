package net.scalax.fsn.common.atomic

import net.scalax.fsn.core.FAtomic

trait FValue[E] extends FAtomic[E] {
  val value: E

  override def toString: String = s"FValue(${value.toString})"
}