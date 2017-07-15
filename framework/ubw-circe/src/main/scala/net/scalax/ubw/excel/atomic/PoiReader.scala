package net.scalax.fsn.excel.atomic

import net.scalax.fsn.core.FAtomic
import org.xarcher.cpoi.ReadableCellOperationAbs

trait PoiReader[E] extends FAtomic[E] {
  type DataType = E

  val reader: ReadableCellOperationAbs[DataType]
}