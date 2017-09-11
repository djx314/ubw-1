package net.scalax.ubw.excel.atomic

import net.scalax.ubw.core.Atomic
import org.xarcher.cpoi.{ StyleTransform, WriteableCellOperationAbs }

trait PoiWriter[E] extends Atomic[E] {
  type DataType = E

  val writer: WriteableCellOperationAbs[DataType]
}

trait PoiStyleTransform[E] extends Atomic[E] {
  type DataType = E
  val transforms: List[StyleTransform]
}