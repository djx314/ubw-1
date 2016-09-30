package indicator.rw.utils.rw2

import indicator.rw.utils.rw._
import net.scalax.fsn.core.FColumn
import net.scalax.ubw.helper.TypeUtils

object InPropertiesConvert {

  def convert(column: FColumn): net.scalax.fsn.model.PropertyInfo = {
    val jsonWriter = FColumn.find(column) { case s: JsonWriter[column.DataType] => s }
    //val slickSelect = FColumn.find(column) { case s: SlickSelect[column.DataType] => s }
    val isInRetrieve = FColumn.findOpt(column) { case s: InRetrieve[column.DataType] => s }.map(_.isInRetrieve).getOrElse(true)
    //val isDefaultDesc = FColumn.findOpt(column) { case s: DefaultDesc[column.DataType] => s }.map(_.isDefaultDesc).getOrElse(true)
    //val orderTargetName = FColumn.findOpt(column) { case s: OrderTargetName[column.DataType] => s }.map(_.orderTargetName)
    val isAutoInc = FColumn.findOpt(column) { case s : AutoInc[column.DataType] => s }.map(_.isAutoInc).getOrElse(false)
    val isPrimary = FColumn.find(column)({ case s: SlickRetrieve[column.DataType] => s }).primaryGen.isDefined
    val property = FColumn.find(column)({ case s: FProperty[column.DataType] => s })

    val slickCommonpropertyInfo: net.scalax.fsn.model.PropertyInfo = net.scalax.fsn.model.PropertyInfo(
      property = property.proName,
      typeName = TypeUtils.unwrapWeakTypeTag(jsonWriter.typeTag.tpe).toString,
      inRetrieve = isInRetrieve,
      //canOrder = false,//slickSelect.colToOrder.isDefined || orderTargetName.isDefined,
      //isDefaultDesc = isDefaultDesc,
      isAutoInc = isAutoInc,
      isPrimaryKey = isPrimary//,
      //selectRender = "",//jsonWriter.selectRender.filterEmptySelect,
      //retrieveRender = "",//jsonWriter.retrieveRender.filterEmptyRetrieve,
      //inputRender = ""//jsonWriter.inputRender.filterEmptyInput
    )
    slickCommonpropertyInfo
  }

  def convertColumn(columns: List[FColumn]): List[net.scalax.fsn.model.PropertyInfo] = {
    columns.map { eachColumn =>
      convert(eachColumn)
    }
  }

  def convertProperty(columns: FColumn): net.scalax.fsn.slick_common.PropertyInfo = {
    val property = FColumn.find(columns) { case s: FProperty[columns.DataType] => s }
    val jsonWriter = FColumn.find(columns) { case s: JsonWriter[columns.DataType] => s }
    val slickSelect = FColumn.find(columns) { case s: SlickSelect[columns.DataType] => s }
    val isDefaultDesc = FColumn.findOpt(columns) { case s: DefaultDesc[columns.DataType] => s }.map(_.isDefaultDesc).getOrElse(true)
    val isInRetrieve = FColumn.findOpt(columns) { case s: InRetrieve[columns.DataType] => s }.map(_.isInRetrieve).getOrElse(true)
    val orderTargetName = FColumn.findOpt(columns) { case s: OrderTargetName[columns.DataType] => s }.map(_.orderTargetName)

    val slickCommonpropertyInfo: net.scalax.fsn.slick_common.PropertyInfo = net.scalax.fsn.slick_common.PropertyInfo(
      property = property.proName,
      typeName = TypeUtils.unwrapWeakTypeTag(jsonWriter.typeTag.tpe).toString,
      inRetrieve = isInRetrieve,
      canOrder = slickSelect.colToOrder.isDefined || orderTargetName.isDefined,
      isDefaultDesc = isDefaultDesc//,
      //selectRender = "",//jsonWriter.selectRender.filterEmptySelect,
      //retrieveRender = "",//jsonWriter.retrieveRender.filterEmptyRetrieve,
      //inputRender = ""//jsonWriter.inputRender.filterEmptyInput
    )

    slickCommonpropertyInfo
  }

}