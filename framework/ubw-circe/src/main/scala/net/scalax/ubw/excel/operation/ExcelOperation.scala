package net.scalax.ubw.json.operation

import net.scalax.ubw.common.atomic.{ DefaultValue, FProperty }
import net.scalax.ubw.core._
import net.scalax.ubw.excel.atomic.{ PoiStyleTransform, PoiWriter }
import org.xarcher.cpoi.CellData
import shapeless._

object ExcelOperation extends AtomicValueHelper {

  /*val writeGen1111 = Pile.transformTreeList {
    new AtomicQuery(_) {
      val aa = withRep(needAtomic[PoiWriter] :: needAtomicOpt[PoiStyleTransform] :: needAtomic[FProperty] :: needAtomicOpt[DefaultValue] :: FANil)
        .mapTo {
          case (poiWriter :: transforms :: property :: defaultOpt :: HNil, data) => {
            val exportData = mergeDefault(defaultOpt, data) //data.opt.fold(defaultOpt.map(_.value))(Option(_))
            val eachColumnData: path.DataType = exportData.getOrElse(throw new Exception(s"字段 ${property.proName} 未被定义"))
            property.proName -> CellData.gen(eachColumnData)(poiWriter.writer).addTransform(transforms.toList.flatMap(_.transforms)): (String, CellData[_])
          }
        }
    }.aa
  } { cellDataTupleList =>
    cellDataTupleList.toMap: Map[String, CellData[_]]
  }*/

  val writeGen = DataPile.transformTree {
    new AtomicQuery(_) {
      val aa = withRep(needAtomic[PoiWriter] :: needAtomicOpt[PoiStyleTransform] :: needAtomic[FProperty] :: needAtomicOpt[DefaultValue] :: FANil)
        .mapTo {
          case (poiWriter :: transforms :: property :: defaultOpt :: HNil, data) => {
            val exportData = mergeDefault(defaultOpt, data) //data.opt.fold(defaultOpt.map(_.value))(Option(_))
            val eachColumnData: path.DataType = exportData.getOrElse(throw new Exception(s"字段 ${property.proName} 未被定义"))
            property.proName -> CellData.gen(eachColumnData)(poiWriter.writer).addTransform(transforms.toList.flatMap(_.transforms)): (String, CellData[_])
          }
        }
    }.aa
  } { (cellDataTupleList, atomicGen) =>
    cellDataTupleList.toMap: Map[String, CellData[_]]
  }

}