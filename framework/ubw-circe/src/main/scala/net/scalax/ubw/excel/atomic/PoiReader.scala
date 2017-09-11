package net.scalax.ubw.excel.atomic

import net.scalax.ubw.core.Atomic
import org.xarcher.cpoi.ReadableCellOperationAbs

trait PoiReader[E] extends Atomic[E] {
  type DataType = E

  val reader: ReadableCellOperationAbs[DataType]
}