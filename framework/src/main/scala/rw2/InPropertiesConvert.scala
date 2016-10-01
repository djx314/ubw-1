package indicator.rw.utils.rw2

import indicator.rw.utils.rw._
import net.scalax.fsn.core.FColumn
import net.scalax.fsn.slick.model.RWProperty
import net.scalax.ubw.helper.TypeUtils

object InPropertiesConvert {

  def convert(column: FColumn): RWProperty = {
    val jsonWriter = FColumn.find(column) { case s: JsonWriter[column.DataType] => s }
    val isInRetrieve = FColumn.findOpt(column) { case s: InRetrieve[column.DataType] => s }.map(_.isInRetrieve).getOrElse(true)
    val isAutoInc = FColumn.findOpt(column) { case s : AutoInc[column.DataType] => s }.map(_.isAutoInc).getOrElse(false)
    val isPrimary = FColumn.find(column)({ case s: SlickRetrieve[column.DataType] => s }).primaryGen.isDefined
    val property = FColumn.find(column)({ case s: FProperty[column.DataType] => s })

    val slickCommonpropertyInfo: RWProperty = RWProperty(
      property = property.proName,
      typeName = TypeUtils.unwrapWeakTypeTag(jsonWriter.typeTag.tpe).toString,
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
      typeName = TypeUtils.unwrapWeakTypeTag(jsonWriter.typeTag.tpe).toString,
      inRetrieve = isInRetrieve,
      canOrder = slickSelect.colToOrder.isDefined || orderTargetName.isDefined,
      isDefaultDesc = isDefaultDesc
    )

    slickCommonpropertyInfo
  }

}