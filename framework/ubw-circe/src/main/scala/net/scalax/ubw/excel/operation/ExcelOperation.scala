package net.scalax.fsn.json.operation

import net.scalax.fsn.common.atomic.{ DefaultValue, FProperty }
import net.scalax.fsn.core._
import net.scalax.fsn.excel.atomic.{ PoiStyleTransform, PoiWriter }
import org.xarcher.cpoi.CellData
import shapeless._

object ExcelOperation extends AtomicValueHelper /*extends AtomicGenHelper with AtomicShapeHelper*/ {

  /*def read(eachColumn: FColumn): Map[String, Json] => FColumn = { data: Map[String, Json] =>
    val jsonReader = FColumn.find(eachColumn)({ case s: JsonReader[eachColumn.DataType] => s })
    val property = FColumn.find(eachColumn)({ case s: FProperty[eachColumn.DataType] => s })
    //lazy val defaultValue = FColumn.findOpt(eachColumn)({ case s: DefaultValue[eachColumn.DataType] => s }).map(_.value)

    /*def getDefaultOrThrow[S <: Exception](e: S): eachColumn.DataType = {
      defaultValue match {
        case Some(s) => s
        case _ => throw e
      }
    }*/

    lazy val fillData = CommonOperation.tryToFillIfEmpty(eachColumn)

    val eachColumnData: FColumn = data.get(property.proName) match {
      case Some(json) =>
        json.as[jsonReader.JsonType](jsonReader.reader).toOption match {
          case Some(data) =>
            FsnColumn(eachColumn.cols, Option(jsonReader.convert(data)))
          case _ =>
            if (fillData.data.isEmpty) {
              throw new Exception(s"字段 ${ property.proName } 的值不能被正确转换")
            }
            fillData
        }
      case None =>
        if (fillData.data.isEmpty) {
          throw new Exception(s"字段 ${ property.proName } 未被定义")
        }
        fillData
    }
    eachColumnData
    //FsnColumn(eachColumn.cols, Option(eachColumnData))
  }

  def readJ(columns: List[FColumn]): Map[String, Json] => List[FColumn] = { data: Map[String, Json] =>
    columns.map { eachColumn =>
      read(eachColumn)(data)
    }
  }

  def readWithFilter(columns: List[FColumn])(filter: FColumn => Boolean): Map[String, Json] => List[FColumn] = { data: Map[String, Json] =>
    columns.map { eachColumn =>
      val isNeed = filter(eachColumn)
      if (isNeed) {
        read(eachColumn)(data)
      } else {
        eachColumn
      }
    }
  }*/
  /*def write(eachColumn: FColumn): (String, CellData[_]) = {
    val poiWriter = FColumn.find(eachColumn)({ case s: PoiWriter[eachColumn.DataType] => s })
    val trans = FColumn.filter(eachColumn)({ case s: PoiStyleTransform[eachColumn.DataType] => s }).map(_.transforms).flatten
    val property = FColumn.find(eachColumn)({ case s: FProperty[eachColumn.DataType] => s })

    val colData = CommonOperation.genDataFromFColumn(eachColumn)
    val eachColumnData: eachColumn.DataType = colData.getOrElse(throw new Exception(s"字段 ${ property.proName } 未被定义"))

    property.proName -> CellData.gen(poiWriter.convert(eachColumnData))(poiWriter.writer).addTransform(trans)
  }

  def writeP(columns: List[FColumn]): Map[String, CellData[_]] = {
    columns.map { eachColumn =>
      write(eachColumn)
    }.toMap
  }*/

  /*val writeGen = Pile.transformTreeList {
    new AtomicQuery(_) {
      val aa = withRep(needAtomic[PoiWriter] :: needAtomicOpt[PoiStyleTransform] :: needAtomic[FProperty] :: needAtomicOpt[DefaultValue] :: FANil)
        .mapToOption {
          case (poiWriter :: transforms :: property :: defaultOpt :: HNil, data) => {
            val exportData = data.fold(defaultOpt.map(_.value))(Option(_))
            val eachColumnData: path.DataType = exportData.getOrElse(throw new Exception(s"字段 ${property.proName} 未被定义"))
            property.proName -> CellData.gen(poiWriter.convert(eachColumnData))(poiWriter.writer).addTransform(transforms.toList.flatMap(_.transforms)): (String, CellData[_])
          }
        }
    }.aa
  } { cellDataTupleList =>
    cellDataTupleList.toMap: Map[String, CellData[_]]
  }*/

  val writeGen = Pile.transformTreeList {
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
  }

}