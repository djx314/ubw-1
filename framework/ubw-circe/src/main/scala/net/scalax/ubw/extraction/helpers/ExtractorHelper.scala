package net.scalax.ubw.extraction.helpers

import net.scalax.fsn.json.operation.AtomicHelper
import net.scalax.ubw.extraction.atomic.Extractor

trait ExtractorHelper[E] extends AtomicHelper[E] {

  def extract(extractor: Extractor[E]) = path.appendAtomic(extractor)

}