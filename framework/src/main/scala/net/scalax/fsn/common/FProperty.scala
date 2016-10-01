package net.scalax.fsn.common

import net.scalax.fsn.core.FAtomic

trait FProperty[E] extends FAtomic[E] {
  val proName: String
}

trait DefaultValue[E] extends FAtomic[E] {
  val value: E
}