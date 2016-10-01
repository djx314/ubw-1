package net.scalax.fsn.excel.atomic

import net.scalax.fsn.core.FAtomic
import org.xarcher.cpoi.WriteableCellOperationAbs

trait PoiWriter[E] extends FAtomic[E] {
  type PoiType
  type DataType = E

  val writer: WriteableCellOperationAbs[PoiType]
  val convert: DataType => PoiType
}