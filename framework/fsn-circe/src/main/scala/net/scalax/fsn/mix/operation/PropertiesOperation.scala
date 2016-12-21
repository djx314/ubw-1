package net.scalax.fsn.mix.operation

import net.scalax.fsn.common.atomic.{DefaultValue, FProperty}
import net.scalax.fsn.core._
import net.scalax.fsn.json.atomic.JsonWriter
import net.scalax.fsn.slick.atomic._
import net.scalax.fsn.slick.model.{RWProperty, SlickParam, SelectProperty, JsonView, JsonOut}
import net.scalax.fsn.slick.helpers.{SlickQueryBindImpl, TypeHelpers}
import net.scalax.fsn.slick.operation.OutSelectConvert
import net.scalax.fsn.slick.operation.OutSelectConvert
import net.scalax.fsn.json.operation.JsonOperation
import shapeless._
import io.circe.syntax._
import io.circe.Json
import slick.basic.BasicProfile
import slick.dbio._
import slick.lifted.{Query, Rep}

import scala.concurrent.ExecutionContext

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

  def slick2jsonOperation(wQuery: SlickQueryBindImpl)(
    implicit
    jsonEv: Query[_, Seq[Any], Seq] => BasicProfile#StreamingQueryActionExtensionMethods[Seq[Seq[Any]], Seq[Any]],
    repToDBIO: Rep[Int] => BasicProfile#QueryActionExtensionMethods[Int, NoStream],
    ec: ExecutionContext
  ): FPileSyntax.PileGen[Option, JsonOut] = {
    val jsonGen: FPileSyntax.PileGen[Option, SlickParam => DBIO[(List[Map[String, Json]], Int)]] = OutSelectConvert.ubwGen(wQuery).flatMap(JsonOperation.writeGen) { (slickQuery, jsonGen) =>
      { slickParam: SlickParam =>
        slickQuery.slickResult.apply(slickParam).map { case (dataList, sum) =>
          dataList.map(s => jsonGen(s).toMap) -> sum
        }
      }
    }

    val propertiesGen: FPileSyntax.PileGen[Option, List[SelectProperty]] = OutSelectConvert.ubwGen(wQuery).flatMap {
      FPile.transformTreeList { path =>
        FAtomicQuery(needAtomic[JsonWriter] :: needAtomic[FProperty] :: needAtomicOpt[DefaultValue] :: needAtomicOpt[DefaultDesc] :: needAtomicOpt[InRetrieve] :: HNil)
        .mapToOption(path) { case (jsonWriter :: property :: defaultOpt :: defaultDescOpt :: inRetrieveOpt :: HNil, data) => {
          val inRetrieve = inRetrieveOpt.map(_.isInRetrieve).getOrElse(true)
          val exportData = data.fold(defaultOpt.map(_.value))(Option(_))
          val eachColumnData: path.DataType = exportData.getOrElse(throw new Exception(s"字段 ${property.proName} 未被定义"))

          (property.proName -> (defaultDescOpt.map(_.isDefaultDesc).getOrElse(true), inRetrieve, jsonWriter.typeTag.tpe.toString))
        } }
      } { jsonTupleList =>
        jsonTupleList
      }
    } { (slickQuery, jsonGen) => {
      val properties = jsonGen(Nil).map { case (proName, (defaultDesc, inRetrieve, typeName)) =>
        SelectProperty(
          proName,
          typeName,
          inRetrieve,
          slickQuery.sortMap.contains(proName),
          defaultDesc
        )
      }
      properties
    } }
    ???
    /*val jsonOutGen: FPileSyntax.PileGen[Option, JsonOut] = jsonGen.flatMap(propertiesGen) { (jsonGen, proGen) =>

    }*/

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