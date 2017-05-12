package net.scalax.fsn.common.atomic

import net.scalax.fsn.core.FAtomic

trait Value[E] extends FAtomic[E] {
  val value: E

  override def toString: String = s"DefaultValue(${value.toString})"
}