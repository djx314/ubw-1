package net.scalax.fsn.mix.operation

import net.scalax.fsn.common.atomic.{DefaultValue, FProperty}
import net.scalax.fsn.core._
import net.scalax.fsn.json.atomic.JsonWriter
import net.scalax.fsn.slick.atomic._
import net.scalax.fsn.slick.model.RWProperty
import net.scalax.fsn.slick.helpers.{SlickQueryBindImpl, TypeHelpers}
import net.scalax.fsn.slick.operation.OutSelectConvert
import net.scalax.fsn.slick.operation.OutSelectConvert.{needAtomic, needAtomicOpt}
import shapeless._
import io.circe.syntax._
import io.circe.Json

object PropertiesOperation extends FAtomicGenHelper with FAtomicShapeHelper with FPilesGenHelper {

  def convert(column: FColumn): RWProperty = {
    val jsonWriter = FColumn.find(column) { case s: JsonWriter[column.DataType] => s }
    val isInRetrieve = FColumn.findOpt(column) { case s: InRetrieve[column.DataType] => s }.map(_.isInRetrieve).getOrElse(true)
    val isAutoInc = FColumn.findOpt(column) { case s : AutoInc[column.DataType] => s }.map(_.isAutoInc).getOrElse(false)
    val isPrimary = FColumn.find(column)({ case s: SlickRetrieve[column.DataType] => s }).primaryGen.isDefined
    val property = FColumn.find(column)({ case s: FProperty[column.DataType] => s })

    val slickCommonpropertyInfo: RWProperty = RWProperty(
      property = property.proName,
      typeName = TypeHelpers.unwrapWeakTypeTag(jsonWriter.typeTag.tpe).toString,
      inRetrieve = isInRetrieve,
      isAutoInc = isAutoInc,
      isPrimaryKey = isPrimary
    )
    slickCommonpropertyInfo
  }

  def convertColumn(columns: List[FColumn]): List[RWProperty] = {
    columns.map { eachColumn =>
      convert(eachColumn)
    }
  }

  def convertProperty(columns: FColumn): net.scalax.fsn.slick.model.SelectProperty = {
    val property = FColumn.find(columns) { case s: FProperty[columns.DataType] => s }
    val jsonWriter = FColumn.find(columns) { case s: JsonWriter[columns.DataType] => s }
    val slickSelect = FColumn.find(columns) { case s: SlickSelect[columns.DataType] => s }
    val isDefaultDesc = FColumn.findOpt(columns) { case s: DefaultDesc[columns.DataType] => s }.map(_.isDefaultDesc).getOrElse(true)
    val isInRetrieve = FColumn.findOpt(columns) { case s: InRetrieve[columns.DataType] => s }.map(_.isInRetrieve).getOrElse(true)
    val orderTargetName = FColumn.findOpt(columns) { case s: OrderTargetName[columns.DataType] => s }.map(_.orderTargetName)

    val slickCommonpropertyInfo: net.scalax.fsn.slick.model.SelectProperty = net.scalax.fsn.slick.model.SelectProperty(
      property = property.proName,
      typeName = TypeHelpers.unwrapWeakTypeTag(jsonWriter.typeTag.tpe).toString,
      inRetrieve = isInRetrieve,
      canOrder = slickSelect.colToOrder.isDefined || orderTargetName.isDefined,
      isDefaultDesc = isDefaultDesc
    )

    slickCommonpropertyInfo
  }

  def slick2jsonOperation(wQuery: SlickQueryBindImpl) = {
    OutSelectConvert.ubwGen(wQuery).flatMap {
      FPile.transformTreeList { path =>
        FAtomicQuery(needAtomic[JsonWriter] :: needAtomic[FProperty] :: needAtomicOpt[DefaultValue] :: needAtomicOpt[DefaultDesc] :: needAtomicOpt[OrderTargetName] :: HNil)
          .mapToOption(path) { case (jsonWriter :: property :: defaultOpt :: defaultDescOpt :: orderTargetNameOpt :: HNil, data) => {
            val exportData = data.fold(defaultOpt.map(_.value))(Option(_))
            val eachColumnData: path.DataType = exportData.getOrElse(throw new Exception(s"字段 ${property.proName} 未被定义"))
            (property.proName -> jsonWriter.convert(eachColumnData).asJson(jsonWriter.writer)) ->
              (property.proName -> (defaultDescOpt.map(_.isDefaultDesc).getOrElse(true)) -> orderTargetNameOpt.map(_.orderTargetName))
          } }
      } { jsonTupleList =>
        val (resultTuple, proInfoTuple) = jsonTupleList.unzip
        (resultTuple.toMap: Map[String, Json]) -> proInfoTuple
      }
    } { ??? }

    /*val jsonGen = FPile.transformTreeList { path: List[FPile[Option]] =>
      FAtomicQuery(needAtomic[JsonWriter] :: needAtomic[FProperty] :: needAtomicOpt[DefaultDesc] :: needAtomicOpt[OrderTargetName] :: needAtomicOpt[DefaultValue] :: HNil)
        .mapToOption(path) { case (jsonWriter :: property :: descParamOpt :: orderTargetNameOpt :: defaultOpt :: HNil, data) => {
          val exportData = data.fold(defaultOpt.map(_.value))(Option(_))
          val eachColumnData: path.DataType = exportData.getOrElse(throw new Exception(s"字段 ${property.proName} 未被定义"))
          property.proName -> jsonWriter.convert(eachColumnData).asJson(jsonWriter.writer)
      } }
    } { jsonTupleList =>
      jsonTupleList.toMap: Map[String, Json]
    }*/
  }

}