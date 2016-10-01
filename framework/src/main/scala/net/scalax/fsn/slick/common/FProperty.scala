package net.scalax.fsn.slick.common

import net.scalax.fsn.core.FAtomic

trait FProperty[E] extends FAtomic[E] {
  val proName: String
}