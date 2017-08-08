package net.scalax.fsn.excel.atomic

import net.scalax.fsn.core.Atomic
import org.xarcher.cpoi.{ StyleTransform, WriteableCellOperationAbs }

trait PoiWriter[E] extends Atomic[E] {
  type DataType = E

  val writer: WriteableCellOperationAbs[DataType]
}

trait PoiStyleTransform[E] extends Atomic[E] {
  type DataType = E
  val transforms: List[StyleTransform]
}