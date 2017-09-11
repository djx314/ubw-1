package net.scalax.ubw.excel.operation

import net.scalax.ubw.excel.atomic.{ PoiReader, PoiStyleTransform, PoiWriter }
import net.scalax.ubw.json.operation.AtomicHelper
import org.xarcher.cpoi.{ ReadableCellOperationAbs, StyleTransform, WriteableCellOperationAbs }

trait ExcelOperation[E] extends AtomicHelper[E] {

  def writeP(implicit writer1: WriteableCellOperationAbs[E]) = path.appendAtomic(new PoiWriter[E] {
    override val writer = writer1
  })

  def readP(implicit reader1: ReadableCellOperationAbs[E]) = path.appendAtomic(new PoiReader[E] {
    override val reader = reader1
  })

  def withTransform(trans: StyleTransform*) = path.appendAtomic(new PoiStyleTransform[E] {
    override val transforms = trans.toList
  })

}