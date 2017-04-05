package net.scalax.fsn.mix.operation

import net.scalax.fsn.common.atomic.{ DefaultValue, FDescribe, FProperty }
import net.scalax.fsn.core._
import net.scalax.fsn.json.atomic.JsonWriter
import net.scalax.fsn.slick.atomic._
import net.scalax.fsn.slick.model._
import net.scalax.fsn.slick.helpers.{ SlickQueryBindImpl, TypeHelpers }
import net.scalax.fsn.slick.operation._
import net.scalax.fsn.json.operation.{ ExcelOperation, JsonOperation }
import net.scalax.fsn.excel.atomic.PoiWriter
import slick.jdbc.JdbcActionComponent
import shapeless._
import io.circe.syntax._
import io.circe.Json
import slick.basic.BasicProfile
import slick.dbio._
import slick.lifted.{ Query, Rep }
import slick.relational.RelationalProfile

import scala.concurrent.Future
import scala.concurrent.ExecutionContext

object PropertiesOperation extends FAtomicGenHelper with FAtomicShapeHelper with FPilesGenHelper {

  val RWPropertiesGen: FPileSyntaxWithoutData.PileGen[Option, List[RWProperty]] = {
    FPile.transformTreeListWithoutData { path =>
      FAtomicQuery(needAtomic[JsonWriter] :: needAtomic[FProperty] :: needAtomicOpt[InRetrieve] :: needAtomicOpt[AutoInc] :: needAtomic[SlickRetrieve] :: HNil)
        .mapToOptionWithoutData(path) {
          case (jsonWriter :: property :: inRetrieveOpt :: isAutoIncOpt :: slickRetrieve :: HNil) =>
            RWProperty(
              property = property.proName,
              typeName = TypeHelpers.unwrapWeakTypeTag(jsonWriter.typeTag.tpe).toString,
              inRetrieve = inRetrieveOpt.map(_.isInRetrieve).getOrElse(true),
              isAutoInc = isAutoIncOpt.map(_.isAutoInc).getOrElse(false),
              isPrimaryKey = slickRetrieve.primaryGen.isDefined
            )
        }
    } { rwProperties =>
      rwProperties
    }
  }
  /*def convert(column: FColumn): RWProperty = {
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
  }*/
  /*def convertProperty(columns: FColumn): net.scalax.fsn.slick.model.SelectProperty = {
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
  }*/

  val strJsonPropertiesGen: FPileSyntaxWithoutData.PileGen[Option, List[SelectProperty]] = StrOutSelectConvert.ubwGenWithoutData.flatMap {
    FPile.transformTreeListWithoutData { path =>
      FAtomicQuery(needAtomic[JsonWriter] :: needAtomic[FProperty] :: needAtomicOpt[FDescribe] :: needAtomicOpt[DefaultValue] :: needAtomicOpt[StrDefaultDesc] :: needAtomicOpt[StrNeededFetch] :: HNil)
        .mapToOptionWithoutData(path) {
          case (jsonWriter :: property :: describeOpt :: defaultOpt :: defaultDescOpt :: neededFetchOpt :: HNil) =>
            val inRetrieve = neededFetchOpt.map(_.isInRetrieve).getOrElse(true)
            (property.proName -> (defaultDescOpt.map(_.isDefaultDesc).getOrElse(true), inRetrieve, TypeHelpers.unwrapWeakTypeTag(jsonWriter.typeTag.tpe).toString, describeOpt.map(_.describe)))
        }
    } { jsonTupleList =>
      jsonTupleList
    }
  } { (sortProNames, jsonGen) =>
    {
      val properties = jsonGen.map {
        case (proName, (defaultDesc, inRetrieve, typeName, describeOpt)) =>
          SelectProperty(
            proName,
            typeName,
            inRetrieve,
            sortProNames.contains(proName),
            defaultDesc,
            describeOpt
          )
      }
      properties
    }
  }

  def strSlick2jsonOperation(wQuery: SlickQueryBindImpl, defaultOrders: List[ColumnOrder])(
    implicit
    jsonEv: Query[_, List[Any], List] => JdbcActionComponent#StreamingQueryActionExtensionMethods[List[List[Any]], List[Any]],
    repToDBIO: Rep[Int] => JdbcActionComponent#QueryActionExtensionMethods[Int, NoStream],
    ec: ExecutionContext
  ): List[FPile[Option]] => JsonOut = { optPiles: List[FPile[Option]] =>

    val jsonGen: FPileSyntax.PileGen[Option, SlickParam => ResultWrap] = StrOutSelectConvert.ubwGen(wQuery).flatMap(JsonOperation.writeGen) { (slickQuery, jsonGen) =>
      { slickParam: SlickParam =>
        val addedParam = slickParam.copy(orders = slickParam.orders ::: defaultOrders)
        val result = slickQuery.slickResult.apply(addedParam)
        val collection = result.resultAction.map {
          case ListAnyCollection(dataList, sum) =>
            ResultCollection(dataList.map(s => jsonGen(s)), sum)
        }
        ResultWrap(collection, result.statements)
      }
    }

    strJsonPropertiesGen.result(optPiles) -> jsonGen.result(optPiles) match {
      case (Left(e1), Left(e2)) => throw e1
      case (Left(e), Right(_)) => throw e
      case (Right(_), Left(e)) => throw e
      case (Right(properties), Right(data)) =>
        JsonOut(properties, data)
    }
  }

  val jsonPropertiesGen: FPileSyntaxWithoutData.PileGen[Option, List[SelectProperty]] = OutSelectConvert.ubwGenWithoutData.flatMap {
    FPile.transformTreeListWithoutData { path =>
      FAtomicQuery(needAtomic[JsonWriter] :: needAtomic[FProperty] :: needAtomicOpt[FDescribe] :: needAtomicOpt[DefaultValue] :: needAtomicOpt[DefaultDesc] :: needAtomicOpt[InRetrieve] :: HNil)
        .mapToOptionWithoutData(path) {
          case (jsonWriter :: property :: describeOpt :: defaultOpt :: defaultDescOpt :: inRetrieveOpt :: HNil) =>
            val inRetrieve = inRetrieveOpt.map(_.isInRetrieve).getOrElse(true)
            (property.proName -> (defaultDescOpt.map(_.isDefaultDesc).getOrElse(true), inRetrieve, TypeHelpers.unwrapWeakTypeTag(jsonWriter.typeTag.tpe).toString, describeOpt.map(_.describe)))
        }
    } { jsonTupleList =>
      jsonTupleList
    }
  } { (sortProNames, jsonGen) =>
    {
      val properties = jsonGen.map {
        case (proName, (defaultDesc, inRetrieve, typeName, describeOpt)) =>
          SelectProperty(
            proName,
            typeName,
            inRetrieve,
            sortProNames.contains(proName),
            defaultDesc,
            describeOpt
          )
      }
      properties
    }
  }

  def slick2jsonOperation(wQuery: SlickQueryBindImpl)(
    implicit
    jsonEv: Query[_, List[Any], List] => JdbcActionComponent#StreamingQueryActionExtensionMethods[List[List[Any]], List[Any]],
    repToDBIO: Rep[Int] => JdbcActionComponent#QueryActionExtensionMethods[Int, NoStream],
    ec: ExecutionContext
  ): List[FPile[Option]] => JsonOut = { optPiles: List[FPile[Option]] =>

    val jsonGen: FPileSyntax.PileGen[Option, SlickParam => ResultWrap] = OutSelectConvert.ubwGen(wQuery).flatMap(JsonOperation.writeGen) { (slickQuery, jsonGen) =>
      { slickParam: SlickParam =>
        ResultWrap(slickQuery.slickResult.apply(slickParam).map {
          case (dataList, sum) =>
            ResultCollection(dataList.map(s => jsonGen(s)), Option(sum))
        }, List.empty[String])
      }
    }

    jsonPropertiesGen.result(optPiles) -> jsonGen.result(optPiles) match {
      case (Left(e1), Left(e2)) => throw e1
      case (Left(e), Right(_)) => throw e
      case (Right(_), Left(e)) => throw e
      case (Right(properties), Right(data)) =>
        JsonOut(properties, data)
    }
  }

  def slick2jsonGroupOperation(wQuery: SlickQueryBindImpl)(
    implicit
    jsonEv: Query[_, List[Any], List] => JdbcActionComponent#StreamingQueryActionExtensionMethods[List[List[Any]], List[Any]],
    //repToDBIO: Rep[Int] => JdbcActionComponent#QueryActionExtensionMethods[Int, NoStream],
    ec: ExecutionContext
  ): List[FPile[Option]] => GroupParam => ResultWrap = { optPiles: List[FPile[Option]] =>

    val jsonGen: FPileSyntax.PileGen[Option, GroupParam => ResultWrap] = GroupSelectConvert.ubwGen(wQuery).flatMap(JsonOperation.unSafewriteGen) { (slickQuery, jsonGen) =>
      { slickParam: GroupParam =>
        val result = slickQuery.result(slickParam)
        ResultWrap(result.action.map {
          case dataList =>
            ResultCollection(dataList.map(s => jsonGen(s)), None)
        }, result.statements)
      }
    }

    jsonGen.result(optPiles) match {
      case Left(e) => throw e
      case Right(s) => s
    }
  }

  val poiPropertiesGen: FPileSyntaxWithoutData.PileGen[Option, List[SelectProperty]] = OutSelectConvert.ubwGenWithoutData /*(wQuery)*/ .flatMap {
    FPile.transformTreeListWithoutData { path =>
      FAtomicQuery(needAtomic[PoiWriter] :: needAtomic[FProperty] :: needAtomicOpt[FDescribe] :: needAtomicOpt[DefaultValue] :: needAtomicOpt[DefaultDesc] :: needAtomicOpt[InRetrieve] :: HNil)
        .mapToOptionWithoutData(path) {
          case (jsonWriter :: property :: describeOpt :: defaultOpt :: defaultDescOpt :: inRetrieveOpt :: HNil) =>
            val inRetrieve = inRetrieveOpt.map(_.isInRetrieve).getOrElse(true)
            (property.proName -> (defaultDescOpt.map(_.isDefaultDesc).getOrElse(true), inRetrieve, jsonWriter.writer.typeTag.tpe.toString, describeOpt.map(_.describe)))
        }
    } { jsonTupleList =>
      jsonTupleList
    }
  } { (sortProNames, jsonGen) =>
    {
      val properties = jsonGen.map {
        case (proName, (defaultDesc, inRetrieve, typeName, describeOpt)) =>
          SelectProperty(
            proName,
            typeName,
            inRetrieve,
            sortProNames.contains(proName),
            defaultDesc,
            describeOpt
          )
      }
      properties
    }
  }

  def slick2PoiOperation(wQuery: SlickQueryBindImpl)(
    implicit
    jsonEv: Query[_, List[Any], List] => JdbcActionComponent#StreamingQueryActionExtensionMethods[List[List[Any]], List[Any]],
    repToDBIO: Rep[Int] => JdbcActionComponent#QueryActionExtensionMethods[Int, NoStream],
    ec: ExecutionContext
  ): List[FPile[Option]] => PoiOut = { optPiles: List[FPile[Option]] =>

    val poiGen /*: FPileSyntax.PileGen[Option, SlickParam => DBIO[(List[Map[String, Json]], Int)]]*/ = OutSelectConvert.ubwGen(wQuery).flatMap(ExcelOperation.writeGen) { (slickQuery, poiGen) =>
      { slickParam: SlickParam =>
        slickQuery.slickResult.apply(slickParam).map {
          case (dataList, sum) =>
            dataList.map(s => poiGen(s)) -> sum
        }
      }
    }

    poiPropertiesGen.result(optPiles) -> poiGen.result(optPiles) match {
      case (Left(e1), Left(e2)) => throw e1
      case (Left(e), Right(_)) => throw e
      case (Right(_), Left(e)) => throw e
      case (Right(properties), Right(data)) =>
        PoiOut(properties, data)
    }
  }

  def json2SlickUpdateOperation(binds: List[(Any, SlickQueryBindImpl)])(
    implicit
    ec: ExecutionContext,
    updateConV: Query[_, Seq[Any], Seq] => JdbcActionComponent#UpdateActionExtensionMethods[Seq[Any]]
  ): List[FPile[Option]] => Map[String, Json] => DBIO[UpdateStaticManyInfo] =
    { optPiles: List[FPile[Option]] =>
      { data: Map[String, Json] =>
        JsonOperation.readGen.flatMap(InUpdateConvert2.updateGen) { (jsonReader, slickWriterGen) =>
          slickWriterGen(jsonReader.apply(data))
        }.flatMap(StaticManyOperation.updateGen) { (execInfoDBIO, staticManyReader) =>
          execInfoDBIO.apply(binds).flatMap { execInfo =>
            for {
              staticMany <- DBIO.from(staticManyReader(execInfo.columns.sortBy(_.index).map(s => Option(s.data))))
            } yield UpdateStaticManyInfo(execInfo.effectRows, staticMany)
          }
        }.result(optPiles).right.get
      }
    }

  def json2SlickDeleteOperation(binds: List[(Any, SlickQueryBindImpl)])(
    implicit
    ec: ExecutionContext,
    deleteConV: Query[RelationalProfile#Table[_], _, Seq] => JdbcActionComponent#DeleteActionExtensionMethods,
    retrieveCv: Query[_, Seq[Any], Seq] => JdbcActionComponent#StreamingQueryActionExtensionMethods[Seq[Seq[Any]], Seq[Any]]
  ): List[FPile[Option]] => Map[String, Json] => DBIO[Int] =
    { optPiles: List[FPile[Option]] =>
      { data: Map[String, Json] =>
        JsonOperation.unfullReadGen.flatMap(InRetrieveConvert2222.convert) { (jsonReader, slickWriterGen) =>
          slickWriterGen(jsonReader.apply(data))
        }.flatMap(StaticManyOperation.updateGen) { (execInfoDBIO, staticManyReader) =>
          execInfoDBIO.apply(binds).flatMap { execInfo =>
            val data = execInfo.columns.sortBy(_.index).map(s => Option(s.data))
            DBIO.from(staticManyReader(data)).flatMap { staticMany =>
              DBIO.sequence(staticMany.map { case (key, query) => query.jsonGen.toView(SlickParam()).flatMap { s => DBIO.sequence(s.data.map { eachData => query.deleteGen(eachData) }) } })
            }.map { s =>
              (s, data)
            }
          }
        }.flatMap(InDeleteConvert2222.convert) {
          case (dataList, slickWriterGen) =>
            dataList.flatMap {
              case (execInfo, data) =>
                slickWriterGen(data).apply(binds).map(_.effectRows)
            }
        }.result(optPiles).right.get
      }
    }

  def json2SlickCreateOperation(binds: List[(Any, SlickQueryBindImpl)])(
    implicit
    ec: ExecutionContext,
    cv: Query[_, Seq[Any], Seq] => JdbcActionComponent#InsertActionExtensionMethods[Seq[Any]],
    retrieveCv: Query[_, Seq[Any], Seq] => JdbcActionComponent#StreamingQueryActionExtensionMethods[Seq[Seq[Any]], Seq[Any]]
  ): List[FPile[Option]] => Map[String, Json] => DBIO[UpdateStaticManyInfo] =
    { optPiles: List[FPile[Option]] =>
      { data: Map[String, Json] =>
        JsonOperation.unfullReadGen.flatMap(InCreateConvert2222.createGen) { (jsonReader, slickWriterGen) =>
          slickWriterGen(jsonReader.apply(data))
        }.flatMap(StaticManyOperation.updateGen) { (execInfoDBIO, staticManyReader) =>
          execInfoDBIO.apply(binds).flatMap { execInfo =>
            for {
              staticMany <- DBIO.from(staticManyReader(execInfo.columns.sortBy(_.index).map(s => Option(s.data))))
            } yield UpdateStaticManyInfo(execInfo.effectRows, staticMany)
          }
        }.result(optPiles).right.get
      }
    }

  def json2SlickRetrieveOperation(binds: List[(Any, SlickQueryBindImpl)])(
    implicit
    ec: ExecutionContext,
    retrieveCv: Query[_, Seq[Any], Seq] => JdbcActionComponent#StreamingQueryActionExtensionMethods[Seq[Seq[Any]], Seq[Any]]
  ): List[FPile[Option]] => Map[String, Json] => DBIO[(Map[String, QueryJsonInfo], Map[String, Json])] =
    { optPiles: List[FPile[Option]] =>
      { data: Map[String, Json] =>
        JsonOperation.unfullReadGen.flatMap(InRetrieveConvert2222.convert) { (jsonReader, slickWriterGen) =>
          slickWriterGen(jsonReader.apply(data))
        }.flatMap(StaticManyOperation.updateGen) { (execInfoDBIO, staticManyReader) =>
          execInfoDBIO.apply(binds).flatMap { execInfo =>
            val rowData = execInfo.columns.sortBy(_.index).map(s => Option(s.data))
            for {
              staticMany <- DBIO.from(staticManyReader(rowData))
            } yield staticMany -> rowData
          }
        }.flatMap(JsonOperation.writeGen) { (statManyWithDataDBIO, jsonWriter) =>
          statManyWithDataDBIO.map {
            case (statMany, rowData) =>
              statMany -> jsonWriter(rowData)
          }
        }.result(optPiles).right.get
      }
    }

  def staticManyOperation(
    implicit
    ec: ExecutionContext
  ): List[FPile[Option]] => Map[String, Json] => Future[Map[String, QueryJsonInfo]] =
    { optPiles: List[FPile[Option]] =>
      { data: Map[String, Json] =>
        JsonOperation.readGen.flatMap(StaticManyOperation.updateGen) { (jsonReader, staticMayGen) =>
          staticMayGen(jsonReader.apply(data))
        }.result(optPiles).right.get
      }
    }

}