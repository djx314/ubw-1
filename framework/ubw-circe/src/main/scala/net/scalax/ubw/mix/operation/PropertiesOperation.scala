package net.scalax.fsn.mix.operation

import net.scalax.fsn.common.atomic.{ DefaultValue, FDescribe, FProperty }
import net.scalax.fsn.core._
import net.scalax.fsn.json.atomic.JsonWriter
import net.scalax.fsn.slick.atomic._
import net.scalax.fsn.slick.model._
import net.scalax.fsn.slick.helpers.{ SlickQueryBindImpl, TypeHelpers }
import net.scalax.fsn.slick.operation._
import net.scalax.fsn.json.operation.{ ExcelOperation, JsonOperation, SlickCompareOperation }
import net.scalax.fsn.excel.atomic.PoiWriter
import slick.jdbc.JdbcActionComponent
import shapeless._
import io.circe.Json
import net.scalax.ubw.validate.atomic.ErrorMessage
import slick.ast.BaseTypedType
import slick.dbio._
import slick.lifted.{ Query, Rep }
import slick.relational.RelationalProfile

import scala.concurrent.Future
import scala.concurrent.ExecutionContext

object PropertiesOperation extends FPilesGenHelper {

  //TODO
  /*val RWPropertiesGen: FPileSyntaxWithoutData.PileGen[List[RWProperty]] = {
    FPile.transformTreeListWithoutData {
      new FAtomicQuery(_) {
        val aa = withRep(needAtomic[JsonWriter] :: needAtomic[FProperty] :: needAtomicOpt[InRetrieve] :: needAtomicOpt[AutoInc] :: needAtomic[SlickRetrieve] :: FANil)
          .mapToWithoutData {
            case (jsonWriter :: property :: inRetrieveOpt :: isAutoIncOpt :: slickRetrieve :: HNil) =>
              RWProperty(
                property = property.proName,
                typeName = "needToFix", //TypeHelpers.unwrapWeakTypeTag(jsonWriter.typeTag.tpe).toString,
                inRetrieve = inRetrieveOpt.map(_.isInRetrieve).getOrElse(true),
                isAutoInc = isAutoIncOpt.map(_.isAutoInc).getOrElse(false),
                isPrimaryKey = slickRetrieve.primaryGen.isDefined
              )
          }
      }.aa
    } { rwProperties =>
      rwProperties
    }
  }*/

  //TODO
  /*val strJsonPropertiesGen: FPileSyntaxWithoutData.PileGen[List[SelectProperty]] = StrOutSelectConvert.ubwGenWithoutData.flatMap {
    FPile.transformTreeListWithoutData {
      new FAtomicQuery(_) {
        val aa = withRep(needAtomic[JsonWriter] :: needAtomic[FProperty] :: needAtomicOpt[FDescribe] :: needAtomicOpt[DefaultValue] :: needAtomicOpt[StrDefaultDesc] :: needAtomicOpt[StrNeededFetch] :: FANil)
          .mapToWithoutData {
            case (jsonWriter :: property :: describeOpt :: defaultOpt :: defaultDescOpt :: neededFetchOpt :: HNil) =>
              val inRetrieve = neededFetchOpt.map(_.isInView).getOrElse(true)
              (property.proName -> (defaultDescOpt.map(_.isDefaultDesc).getOrElse(true), inRetrieve, "needToFix", /*TypeHelpers.unwrapWeakTypeTag(jsonWriter.typeTag.tpe).toString*/ describeOpt.map(_.describe)))
          }
      }.aa
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
  }*/

  def filterStrSlick2jsonOperation(wQuery: SlickQueryBindImpl, defaultOrders: List[ColumnOrder])(
    implicit
    jsonEv: Query[_, List[Any], List] => JdbcActionComponent#StreamingQueryActionExtensionMethods[List[List[Any]], List[Any]],
    repToDBIO: Rep[Int] => JdbcActionComponent#QueryActionExtensionMethods[Int, NoStream],
    ec: ExecutionContext
  ): List[FPile] => JsonOut = { optPiles: List[FPile] =>

    val jsonFilter: FPileSyntax.PileGen[SlickParam => StrSlickQuery] =
      SlickCompareOperation.unfullReadCompareGen.flatMap(StrOutSelectConvert.ubwGen(wQuery)) { (jsonGen, slickQuery) =>
        { slickParam: SlickParam =>
          slickQuery(jsonGen(slickParam.filter))
        }
      }
    val jsonGen: FPileSyntax.PileGen[SlickParam => ResultWrap] = jsonFilter.flatMap(JsonOperation.unSafewriteGen1111) { (slickQuery, jsonGen) =>
      { slickParam: SlickParam =>
        val addedParam = slickParam.copy(orders = slickParam.orders ::: defaultOrders)
        val result = slickQuery(slickParam).slickResult.apply(addedParam)
        val collection = result.resultAction.map {
          case ListAnyCollection1111(dataList, sum) =>
            ResultCollection(dataList.map(s => jsonGen(s)), sum)
        }
        ResultWrap(collection, result.statements)
      }
    }

    /*strJsonPropertiesGen.result(optPiles)*/ (Right(Nil): Either[Exception, List[SelectProperty]]) -> jsonGen.result(optPiles) match {
      case (Left(e1), Left(e2)) => throw e1
      case (Left(e), Right(_)) => throw e
      case (Right(_), Left(e)) => throw e
      case (Right(properties), Right(data)) =>
        JsonOut(properties, data)
    }
  }

  def strSlick2jsonOperation(wQuery: SlickQueryBindImpl, defaultOrders: List[ColumnOrder])(
    implicit
    jsonEv: Query[_, List[Any], List] => JdbcActionComponent#StreamingQueryActionExtensionMethods[List[List[Any]], List[Any]],
    repToDBIO: Rep[Int] => JdbcActionComponent#QueryActionExtensionMethods[Int, NoStream],
    ec: ExecutionContext
  ): List[FPile] => JsonOut = { optPiles: List[FPile] =>
    val jsonGen: FPileSyntax.PileGen[SlickParam => ResultWrap] = StrOutSelectConvert.ubwGen(wQuery).flatMap(JsonOperation.unSafewriteGen1111) { (slickQuery, jsonGen) =>
      { slickParam: SlickParam =>
        val addedParam = slickParam.copy(orders = slickParam.orders ::: defaultOrders)
        val result = slickQuery.slickResult.apply(addedParam)
        val collection = result.resultAction.map {
          case ListAnyCollection1111(dataList, sum) =>
            ResultCollection(dataList.map(s => jsonGen(s)), sum)
        }
        ResultWrap(collection, result.statements)
      }
    }

    /*strJsonPropertiesGen.result(optPiles)*/ (Right(Nil): Either[Exception, List[SelectProperty]]) -> jsonGen.result(optPiles) match {
      case (Left(e1), Left(e2)) => throw e1
      case (Left(e), Right(_)) => throw e
      case (Right(_), Left(e)) => throw e
      case (Right(properties), Right(data)) =>
        JsonOut(properties, data)
    }
  }

  /*val jsonPropertiesGen: FPileSyntaxWithoutData.PileGen[Option, List[SelectProperty]] = OutSelectConvert.ubwGenWithoutData.flatMap {
    FPile.transformTreeListWithoutData {
      new FAtomicQuery(_) {
        val aa = withRep(needAtomic[JsonWriter] :: needAtomic[FProperty] :: needAtomicOpt[FDescribe] :: needAtomicOpt[DefaultValue] :: needAtomicOpt[DefaultDesc] :: needAtomicOpt[InRetrieve] :: FANil)
          .mapToOptionWithoutData {
            case (jsonWriter :: property :: describeOpt :: defaultOpt :: defaultDescOpt :: inRetrieveOpt :: HNil) =>
              val inRetrieve = inRetrieveOpt.map(_.isInRetrieve).getOrElse(true)
              (property.proName -> (defaultDescOpt.map(_.isDefaultDesc).getOrElse(true), inRetrieve, TypeHelpers.unwrapWeakTypeTag(jsonWriter.typeTag.tpe).toString, describeOpt.map(_.describe)))
          }
      }.aa
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
  }*/

  /*def slick2jsonOperation(wQuery: SlickQueryBindImpl)(
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
  }*/

  def slick2jsonGroupOperation(wQuery: SlickQueryBindImpl)(
    implicit
    jsonEv: Query[_, List[Any], List] => JdbcActionComponent#StreamingQueryActionExtensionMethods[List[List[Any]], List[Any]],
    intTyped: BaseTypedType[Int],
    //repToDBIO: Rep[Int] => JdbcActionComponent#QueryActionExtensionMethods[Int, NoStream],
    ec: ExecutionContext
  ): List[FPile] => GroupParam => ResultWrap = { optPiles: List[FPile] =>

    val jsonGen: FPileSyntax.PileGen[GroupParam => ResultWrap] = GroupSelectConvert.ubwGen(wQuery).flatMap(JsonOperation.unSafewriteGen1111) { (slickQuery, jsonGen) =>
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

  //TODO
  /*val poiPropertiesGen: FPileSyntaxWithoutData.PileGen[List[SelectProperty]] = StrOutSelectConvert.ubwGenWithoutData /*(wQuery)*/ .flatMap {
    FPile.transformTreeListWithoutData {
      new FAtomicQuery(_) {
        val aa = withRep(needAtomic[PoiWriter] :: needAtomic[FProperty] :: needAtomicOpt[FDescribe] :: needAtomicOpt[DefaultValue] :: needAtomicOpt[DefaultDesc] :: needAtomicOpt[InRetrieve] :: FANil)
          .mapToWithoutData {
            case (jsonWriter :: property :: describeOpt :: defaultOpt :: defaultDescOpt :: inRetrieveOpt :: HNil) =>
              val inRetrieve = inRetrieveOpt.map(_.isInRetrieve).getOrElse(true)
              (property.proName -> (defaultDescOpt.map(_.isDefaultDesc).getOrElse(true), inRetrieve, jsonWriter.writer.typeTag.tpe.toString, describeOpt.map(_.describe)))
          }
      }.aa
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
  }*/

  def slick2PoiOperation(wQuery: SlickQueryBindImpl)(
    implicit
    jsonEv: Query[_, List[Any], List] => JdbcActionComponent#StreamingQueryActionExtensionMethods[List[List[Any]], List[Any]],
    repToDBIO: Rep[Int] => JdbcActionComponent#QueryActionExtensionMethods[Int, NoStream],
    ec: ExecutionContext
  ): List[FPile] => PoiOut = { optPiles: List[FPile] =>

    val poiGen /*: FPileSyntax.PileGen[Option, SlickParam => DBIO[(List[Map[String, Json]], Int)]]*/ = StrOutSelectConvert.ubwGen(wQuery).flatMap(ExcelOperation.writeGen1111) { (slickQuery, poiGen) =>
      { slickParam: SlickParam =>
        slickQuery.slickResult.apply(slickParam).resultAction.map {
          case ListAnyCollection1111(dataList, sum) =>
            //TODO Remove None.get
            dataList.map(s => poiGen(s)) -> sum.get
        }
      }
    }

    /*poiPropertiesGen.result(optPiles)*/ (Right(Nil): Either[Exception, List[SelectProperty]]) -> poiGen.result(optPiles) match {
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
  ): List[FPile] => Map[String, Json] => Future[Either[List[ErrorMessage], DBIO[UpdateStaticManyInfo]]] =
    { optPiles: List[FPile] =>
      { data: Map[String, Json] =>
        JsonOperation.unfullReadGen1111.flatMap(InUpdateConvert.updateGen) { (jsonReader, slickWriterGen) =>
          slickWriterGen(jsonReader.apply(data))
        }.flatMap(StaticManyOperation.updateGen) {
          case ((execInfoDBIOF, validateInfoF), staticManyReader) =>
            validateInfoF.map { validateInfo =>
              if (!validateInfo.isEmpty) {
                Left(validateInfo)
              } else {
                Right(execInfoDBIOF.apply(binds).map { execInfoDBIO =>
                  execInfoDBIO.flatMap { execInfo =>
                    for {
                      staticMany <- DBIO.from(staticManyReader(execInfo.columns.sortBy(_.index).map(s => s.data)))
                    } yield UpdateStaticManyInfo(execInfo.effectRows, staticMany)
                  }
                })
              }
            }
        }.result(optPiles) match {
          case Left(e: Exception) =>
            e.printStackTrace()
            throw e
          case Right(s) => s.flatMap {
            case Left(messages) => Future.successful(Left(messages))
            case Right(t) => t.map(r => Right(r))
          }
        }
      }
    }

  def json2SlickDeleteOperation(binds: List[(Any, SlickQueryBindImpl)])(
    implicit
    ec: ExecutionContext,
    deleteConV: Query[RelationalProfile#Table[_], _, Seq] => JdbcActionComponent#DeleteActionExtensionMethods,
    retrieveCv: Query[_, Seq[Any], Seq] => JdbcActionComponent#StreamingQueryActionExtensionMethods[Seq[Seq[Any]], Seq[Any]]
  ): List[FPile] => Map[String, Json] => DBIO[Int] =
    { optPiles: List[FPile] =>
      { data: Map[String, Json] =>
        JsonOperation.unfullReadGen1111.flatMap(InRetrieveConvert.convert) { (jsonReader, slickWriterGen) =>
          slickWriterGen(jsonReader.apply(data))
        }.flatMap(StaticManyOperation.updateGen) { (execInfoDBIO, staticManyReader) =>
          execInfoDBIO.apply(binds).flatMap { execInfo =>
            val data = execInfo.columns.sortBy(_.index).map(s => s.data)
            DBIO.from(staticManyReader(data)).flatMap { staticMany =>
              DBIO.sequence(staticMany.map { case (key, query) => query.jsonGen.toView(SlickParam()).flatMap { s => DBIO.sequence(s.data.map { eachData => query.deleteGen(eachData) }) } })
            }.map { s =>
              (s, data)
            }
          }
        }.flatMap(InDeleteConvert.convert) {
          case (dataList, slickWriterGen) =>
            dataList.flatMap {
              case (execInfo, data) =>
                slickWriterGen(data).apply(binds).map(_.effectRows)
            }
        }.result(optPiles) match {
          case Left(e: Exception) =>
            e.printStackTrace()
            throw e
          case Right(s) => s
        }
      }
    }

  def json2SlickCreateOperation(binds: List[(Any, SlickQueryBindImpl)])(
    implicit
    ec: ExecutionContext,
    cv: Query[_, Seq[Any], Seq] => JdbcActionComponent#InsertActionExtensionMethods[Seq[Any]],
    retrieveCv: Query[_, Seq[Any], Seq] => JdbcActionComponent#StreamingQueryActionExtensionMethods[Seq[Seq[Any]], Seq[Any]]
  ): List[FPile] => Map[String, Json] => DBIO[UpdateStaticManyInfo] =
    { optPiles: List[FPile] =>
      { data: Map[String, Json] =>
        JsonOperation.unfullReadGen.flatMap(InCreateConvert.createGen) { (jsonReader, slickWriterGen) =>
          slickWriterGen(jsonReader.apply(data))
        }.flatMap(StaticManyOperation.updateGen) { (execInfoDBIO, staticManyReader) =>
          execInfoDBIO.apply(binds).flatMap { execInfo =>
            for {
              staticMany <- DBIO.from(staticManyReader(execInfo.columns.sortBy(_.index).map(s => s.data)))
            } yield UpdateStaticManyInfo(execInfo.effectRows, staticMany)
          }
        }.result(optPiles) match {
          case Left(e: Exception) =>
            e.printStackTrace()
            throw e
          case Right(s) => s
        }
      }
    }

  def json2SlickRetrieveOperation(binds: List[(Any, SlickQueryBindImpl)])(
    implicit
    ec: ExecutionContext,
    retrieveCv: Query[_, Seq[Any], Seq] => JdbcActionComponent#StreamingQueryActionExtensionMethods[Seq[Seq[Any]], Seq[Any]]
  ): List[FPile] => Map[String, Json] => DBIO[(Map[String, QueryJsonInfo], Map[String, Json])] =
    { optPiles: List[FPile] =>
      { data: Map[String, Json] =>
        JsonOperation.unfullReadGen1111.flatMap(InRetrieveConvert.convert) { (jsonReader, slickWriterGen) =>
          slickWriterGen(jsonReader.apply(data))
        }.flatMap(StaticManyOperation.updateGen) { (execInfoDBIO, staticManyReader) =>
          execInfoDBIO.apply(binds).flatMap { execInfo =>
            val rowData = execInfo.columns.sortBy(_.index).map(s => s.data)
            for {
              staticMany <- DBIO.from(staticManyReader(rowData))
            } yield staticMany -> rowData
          }
        }.flatMap(JsonOperation.writeGen1111) { (statManyWithDataDBIO, jsonWriter) =>
          statManyWithDataDBIO.map {
            case (statMany, rowData) =>
              statMany -> jsonWriter(rowData)
          }
        }.result(optPiles) match {
          case Left(e: Exception) =>
            e.printStackTrace()
            throw e
          case Right(s) => s
        }
      }
    }

  def staticManyOperation(
    implicit
    ec: ExecutionContext
  ): List[FPile] => Map[String, Json] => Future[Map[String, QueryJsonInfo]] =
    { optPiles: List[FPile] =>
      { data: Map[String, Json] =>
        JsonOperation.readGen1111.flatMap(StaticManyOperation.updateGen) { (jsonReader, staticMayGen) =>
          staticMayGen(jsonReader.apply(data))
        }.result(optPiles) match {
          case Left(e: Exception) =>
            e.printStackTrace()
            throw e
          case Right(s) => s
        }
      }
    }

}