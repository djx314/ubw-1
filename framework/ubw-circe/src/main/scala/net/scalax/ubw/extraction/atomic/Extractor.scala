package net.scalax.ubw.extraction.atomic

import net.scalax.ubw.core.Atomic

trait Extractor[E] extends Atomic[E] {
  override def toString: String = {
    s"Extractor(${super.toString})"
  }
}

object Extractor {
  def apply[E]: Extractor[E] = new Extractor[E] {}
}