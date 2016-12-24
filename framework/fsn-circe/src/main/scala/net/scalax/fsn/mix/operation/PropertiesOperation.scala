package net.scalax.fsn.mix.operation

import net.scalax.fsn.common.atomic.{DefaultValue, FProperty}
import net.scalax.fsn.core._
import net.scalax.fsn.json.atomic.JsonWriter
import net.scalax.fsn.slick.atomic._
import net.scalax.fsn.slick.model.{JsonOut, PoiOut, JsonView, RWProperty, SelectProperty, SlickParam}
import net.scalax.fsn.slick.helpers.{SlickQueryBindImpl, TypeHelpers}
import net.scalax.fsn.slick.operation.OutSelectConvert
import net.scalax.fsn.slick.operation.OutSelectConvert
import net.scalax.fsn.json.operation.{ExcelOperation, JsonOperation}
import net.scalax.fsn.excel.atomic.PoiWriter
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
  ): List[FPile[Option]] => JsonOut = { optPiles: List[FPile[Option]] =>

    val jsonGen: FPileSyntax.PileGen[Option, SlickParam => DBIO[(List[Map[String, Json]], Int)]] = OutSelectConvert.ubwGen(wQuery).flatMap(JsonOperation.writeGen) { (slickQuery, jsonGen) =>
      { slickParam: SlickParam =>
        slickQuery.slickResult.apply(slickParam).map { case (dataList, sum) =>
          dataList.map(s => jsonGen(s).toMap) -> sum
        }
      }
    }

    val propertiesGen: FPileSyntaxWithoutData.PileGen[Option, List[SelectProperty]] = OutSelectConvert.ubwGenWithoutData/*(wQuery)*/.flatMap {
      FPile.transformTreeListWithoutData { path =>
        FAtomicQuery(needAtomic[JsonWriter] :: needAtomic[FProperty] :: needAtomicOpt[DefaultValue] :: needAtomicOpt[DefaultDesc] :: needAtomicOpt[InRetrieve] :: HNil)
        .mapToOptionWithoutData(path) { case (jsonWriter :: property :: defaultOpt :: defaultDescOpt :: inRetrieveOpt :: HNil) =>
          val inRetrieve = inRetrieveOpt.map(_.isInRetrieve).getOrElse(true)
          (property.proName -> (defaultDescOpt.map(_.isDefaultDesc).getOrElse(true), inRetrieve, TypeHelpers.unwrapWeakTypeTag(jsonWriter.typeTag.tpe).toString))
        }
      } { jsonTupleList =>
        jsonTupleList
      }
    } { (sortProNames, jsonGen) => {
      val properties = jsonGen.map { case (proName, (defaultDesc, inRetrieve, typeName)) =>
        SelectProperty(
          proName,
          typeName,
          inRetrieve,
          sortProNames.contains(proName),
          defaultDesc
        )
      }
      properties
    } }

    propertiesGen.result(optPiles) -> jsonGen.result(optPiles) match {
      case (Left(e1), Left(e2)) => throw e1
      case (Left(e), Right(_)) => throw e
      case (Right(_), Left(e)) => throw e
      case (Right(properties), Right(data)) =>
        JsonOut(properties, data)
    }
  }

  def slick2PoiOperation(wQuery: SlickQueryBindImpl)(
    implicit
    jsonEv: Query[_, Seq[Any], Seq] => BasicProfile#StreamingQueryActionExtensionMethods[Seq[Seq[Any]], Seq[Any]],
    repToDBIO: Rep[Int] => BasicProfile#QueryActionExtensionMethods[Int, NoStream],
    ec: ExecutionContext
  ): List[FPile[Option]] => PoiOut = { optPiles: List[FPile[Option]] =>

    val poiGen/*: FPileSyntax.PileGen[Option, SlickParam => DBIO[(List[Map[String, Json]], Int)]]*/= OutSelectConvert.ubwGen(wQuery).flatMap(ExcelOperation.writeGen) { (slickQuery, poiGen) =>
      { slickParam: SlickParam =>
        slickQuery.slickResult.apply(slickParam).map { case (dataList, sum) =>
          dataList.map(s => poiGen(s).toMap) -> sum
        }
      }
    }

    val propertiesGen: FPileSyntaxWithoutData.PileGen[Option, List[SelectProperty]] = OutSelectConvert.ubwGenWithoutData/*(wQuery)*/.flatMap {
      FPile.transformTreeListWithoutData { path =>
        FAtomicQuery(needAtomic[PoiWriter] :: needAtomic[FProperty] :: needAtomicOpt[DefaultValue] :: needAtomicOpt[DefaultDesc] :: needAtomicOpt[InRetrieve] :: HNil)
          .mapToOptionWithoutData(path) { case (jsonWriter :: property :: defaultOpt :: defaultDescOpt :: inRetrieveOpt :: HNil) =>
            val inRetrieve = inRetrieveOpt.map(_.isInRetrieve).getOrElse(true)
            (property.proName -> (defaultDescOpt.map(_.isDefaultDesc).getOrElse(true), inRetrieve, jsonWriter.writer.typeTag.tpe.toString))
          }
      } { jsonTupleList =>
        jsonTupleList
      }
    } { (sortProNames, jsonGen) => {
      val properties = jsonGen.map { case (proName, (defaultDesc, inRetrieve, typeName)) =>
        SelectProperty(
          proName,
          typeName,
          inRetrieve,
          sortProNames.contains(proName),
          defaultDesc
        )
      }
      properties
    } }

    propertiesGen.result(optPiles) -> poiGen.result(optPiles) match {
      case (Left(e1), Left(e2)) => throw e1
      case (Left(e), Right(_)) => throw e
      case (Right(_), Left(e)) => throw e
      case (Right(properties), Right(data)) =>
        PoiOut(properties, data)
    }
  }

}