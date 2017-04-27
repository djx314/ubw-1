package net.scalax.fsn.excel.atomic

import net.scalax.fsn.core.FAtomic
import org.xarcher.cpoi.ReadableCellOperationAbs

trait PoiReader[E] extends FAtomic[E] {
  type PoiType
  type DataType = E

  val reader: ReadableCellOperationAbs[PoiType]
  val convert: PoiType => DataType
}