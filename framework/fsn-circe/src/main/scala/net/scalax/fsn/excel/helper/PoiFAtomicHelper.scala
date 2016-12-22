package net.scalax.fsn.excel.operation

import net.scalax.fsn.excel.atomic.{PoiReader, PoiStyleTransform, PoiWriter}
import net.scalax.fsn.json.operation.FAtomicHelper
import org.xarcher.cpoi.{ReadableCellOperationAbs, StyleTransform, WriteableCellOperationAbs}

trait ExcelOperation[E] extends FAtomicHelper[E] {

  def writeP(implicit writer1: WriteableCellOperationAbs[E]) = append(new PoiWriter[E] {
    override type PoiType = E
    override val writer = writer1
    override val convert = identity[E] _
  })

  def readP(implicit reader1: ReadableCellOperationAbs[E]) = append(new PoiReader[E] {
    override type PoiType = E
    override val reader = reader1
    override val convert = identity[E] _
  })

  def withTransform(trans: StyleTransform*) = append(new PoiStyleTransform[E] {
    override val transforms = trans.toList
  })

}