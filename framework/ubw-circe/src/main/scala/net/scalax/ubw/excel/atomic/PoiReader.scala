package net.scalax.fsn.excel.atomic

import net.scalax.fsn.core.Atomic
import org.xarcher.cpoi.ReadableCellOperationAbs

trait PoiReader[E] extends Atomic[E] {
  type DataType = E

  val reader: ReadableCellOperationAbs[DataType]
}