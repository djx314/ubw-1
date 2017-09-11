package net.scalax.ubw.extraction.helpers

import net.scalax.ubw.json.operation.AtomicHelper
import net.scalax.ubw.extraction.atomic.Extractor

trait ExtractorHelper[E] extends AtomicHelper[E] {

  def extract(extractor: Extractor[E]) = path.appendAtomic(extractor)

}