package net.scalax.fsn.excel.atomic

import net.scalax.fsn.core.FAtomic
import org.xarcher.cpoi.{ StyleTransform, WriteableCellOperationAbs }

trait PoiWriter[E] extends FAtomic[E] {
  type DataType = E

  val writer: WriteableCellOperationAbs[DataType]
}

trait PoiStyleTransform[E] extends FAtomic[E] {
  type DataType = E
  val transforms: List[StyleTransform]
}