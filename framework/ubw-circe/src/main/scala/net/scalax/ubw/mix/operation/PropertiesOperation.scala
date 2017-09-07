package net.scalax.fsn.mix.operation

import net.scalax.fsn.core._
import net.scalax.fsn.slick.model._
import net.scalax.fsn.slick.helpers.SlickQueryBindImpl
import net.scalax.fsn.slick.operation._
import net.scalax.fsn.json.operation.JsonOperation
import slick.jdbc.JdbcProfile
import io.circe.Json
import net.scalax.ubw.validate.atomic.ErrorMessage
import slick.basic.BasicBackend
import slick.dbio._

import scala.concurrent.Future
import scala.concurrent.ExecutionContext

object PropertiesOperation extends PilesGenHelper {

  //TODO
  /*val RWPropertiesGen: PileSyntaxWithoutData.PileGen[List[RWProperty]] = {
    Pile.transformTreeListWithoutData {
      new AtomicQuery(_) {
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
  /*val strJsonPropertiesGen: PileSyntaxWithoutData.PileGen[List[SelectProperty]] = StrOutSelectConvert.ubwGenWithoutData.flatMap {
    Pile.transformTreeListWithoutData {
      new AtomicQuery(_) {
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

  def filterStrSlick2jsonOperation(wQuery: SlickQueryBindImpl, slickParam: SlickParam)(
    implicit
    slickProfile: JdbcProfile,
    ec: ExecutionContext
  ): List[Pile] => Map[String, Json] => JsonOut = { optPiles: List[Pile] =>
    { data: Map[String, Json] =>

      val jsonFilter = JsonOperation.unfullreadGen1111.next(StrOutSelectConvert1111.ubwGen(wQuery, slickParam.copy(filter = slickParam.filter ++ data)))(JsonOperation.vFunctor).next(JsonOperation.unSafewriteGen1111)

      jsonFilter.result(optPiles) match {
        case Right(slickOperation) =>
          JsonOut(slickOperation(data))
        case Left(e) => throw e
      }
    }
  }

  /*def strSlick2jsonOperation1111(wQuery: SlickQueryBindImpl, slickParam: SlickParam)(
    implicit
    slickProfile: JdbcProfile,
    ec: ExecutionContext
  ): List[Pile] => JsonOut = { optPiles: List[Pile] =>
    val jsonGen: PileSyntax.PileGen[SlickParam => ListAnyWrap3333[Map[String, Json]]] = StrOutSelectConvert.ubwGen(wQuery).flatMap(JsonOperation.unSafewriteGen) { (slickQuery, jsonGen) =>
      val addedParam = slickParam.copy(orders = slickParam.orders ::: defaultOrders)
      val result = slickQuery.slickResult.apply(addedParam)
      val collection = result.resultAction.map {
        case ListAnyCollection3333(dataList, sum) =>
          ListAnyCollection3333(dataList.map(s => jsonGen(s)), sum)
      }
      ListAnyWrap3333(collection, result.statements)
    }

    (Right(Nil): Either[Exception, List[SelectProperty]]) -> jsonGen.result(optPiles) match {
      case (Left(e1), Left(e2)) => throw e1
      case (Left(e), Right(_)) => throw e
      case (Right(_), Left(e)) => throw e
      case (Right(properties), Right(data)) =>
        JsonOut(data)
    }
  }*/

  def strSlick2jsonOperation(wQuery: SlickQueryBindImpl, slickParam: SlickParam)(
    implicit
    slickProfile: JdbcProfile,
    ec: ExecutionContext
  ): List[Pile] => JsonOut = { optPiles: List[Pile] =>
    val jsonGen = StrOutSelectConvert1111.ubwGen(wQuery, slickParam).next(JsonOperation.unSafewriteGen1111)
    /*{ (slickQuery, jsonGen) =>
      { slickParam: SlickParam =>
        val addedParam = slickParam.copy(orders = slickParam.orders ::: defaultOrders)
        val result = slickQuery.slickResult.apply(addedParam)
        val collection = result.resultAction.map {
          case ListAnyCollection2222(dataList, sum) =>
            ResultCollection(dataList.map(s => jsonGen(s)), sum)
        }
        ResultWrap(collection, result.statements)
      }
    }*/

    jsonGen.result(optPiles) match {
      case Right(data) =>
        JsonOut(data)
      case Left(e1) => throw e1

    }
  }

  /*val jsonPropertiesGen: PileSyntaxWithoutData.PileGen[Option, List[SelectProperty]] = OutSelectConvert.ubwGenWithoutData.flatMap {
    Pile.transformTreeListWithoutData {
      new AtomicQuery(_) {
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
  ): List[Pile[Option]] => JsonOut = { optPiles: List[Pile[Option]] =>

    val jsonGen: PileSyntax.PileGen[Option, SlickParam => ResultWrap] = OutSelectConvert.ubwGen(wQuery).flatMap(JsonOperation.writeGen) { (slickQuery, jsonGen) =>
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
    //jsonEv: Query[_, List[Any], List] => JdbcActionComponent#StreamingQueryActionExtensionMethods[List[List[Any]], List[Any]],
    //intTyped: BaseTypedType[Int],
    //repToDBIO: Rep[Int] => JdbcActionComponent#QueryActionExtensionMethods[Int, NoStream],
    slickProfile: JdbcProfile,
    ec: ExecutionContext
  ): List[Pile] => GroupParam => ListAnyWrap3333[Map[String, Json]] = { optPiles: List[Pile] =>
    val jsonGen: PileSyntax.PileGen[GroupParam => ListAnyWrap3333[Map[String, Json]]] = GroupSelectConvert.ubwGen(wQuery).flatMap(JsonOperation.unSafewriteGen) { (slickQuery, jsonGen) =>
      { slickParam: GroupParam =>
        val result = slickQuery.result(slickParam)
        ListAnyWrap3333(result.action.map {
          case dataList =>
            ListAnyCollection3333(dataList.map(s => jsonGen(s)), None)
        }, result.statements)
      }
    }

    jsonGen.result(optPiles) match {
      case Left(e) => throw e
      case Right(s) => s
    }
  }

  //TODO
  /*val poiPropertiesGen: PileSyntaxWithoutData.PileGen[List[SelectProperty]] = StrOutSelectConvert.ubwGenWithoutData /*(wQuery)*/ .flatMap {
    Pile.transformTreeListWithoutData {
      new AtomicQuery(_) {
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

  /*def slick2PoiOperation(wQuery: SlickQueryBindImpl)(
    implicit
    slickProfile: JdbcProfile,
    ec: ExecutionContext
  ): List[Pile] => PoiOut = { optPiles: List[Pile] =>
    val poiGen /*: PileSyntax.PileGen[Option, SlickParam => DBIO[(List[Map[String, Json]], Int)]]*/ = StrOutSelectConvert1111.ubwGen(wQuery).flatMap(ExcelOperation.writeGen) { (slickQuery, poiGen) =>
      { slickParam: SlickParam =>
        slickQuery.slickResult.apply(slickParam).resultAction.map {
          case ListAnyCollection3333(dataList, sum) =>
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
  }*/

  def json2SlickUpdateOperation(binds: List[(Any, SlickQueryBindImpl)])(
    implicit
    slickProfile: JdbcProfile,
    ec: ExecutionContext
  ): List[Pile] => Map[String, Json] => Future[Either[List[ErrorMessage], DBIO[UpdateStaticManyInfo]]] =
    { optPiles: List[Pile] =>
      { data: Map[String, Json] =>
        JsonOperation.unfullreadGen.flatMap(InUpdateConvert.updateGen) { (jsonReader, slickWriterGen) =>
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
    slickProfile: JdbcProfile,
    ec: ExecutionContext
  ): List[Pile] => Map[String, Json] => DBIO[Int] =
    { optPiles: List[Pile] =>
      { data: Map[String, Json] =>
        JsonOperation.unfullreadGen.flatMap(InRetrieveConvert.convert) { (jsonReader, slickWriterGen) =>
          slickWriterGen(jsonReader.apply(data))
        }.flatMap(StaticManyOperation.updateGen) { (execInfoDBIO, staticManyReader) =>
          execInfoDBIO.apply(binds).flatMap { execInfo =>
            val data = execInfo.columns.sortBy(_.index).map(s => s.data)
            DBIO.from(staticManyReader(data)).flatMap { staticMany =>
              DBIO.sequence(staticMany.map { case (key, query) => query.jsonGen.toView.flatMap { s => DBIO.sequence(s.data.map { eachData => query.deleteGen(eachData) }) } })
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
    slickProfile: JdbcProfile,
    ec: ExecutionContext
  ): List[Pile] => Map[String, Json] => DBIO[UpdateStaticManyInfo] =
    { optPiles: List[Pile] =>
      { data: Map[String, Json] =>
        JsonOperation.unfullreadGen.flatMap(InCreateConvert.createGen) { (jsonReader, slickWriterGen) =>
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

  def slick_retrieve_i_slick_update_or_insert_o(binds: List[(Any, SlickQueryBindImpl)], db: BasicBackend#Database)(
    implicit
    ec: ExecutionContext,
    slickProfile: JdbcProfile
  ): List[Pile] => Future[ExecInfo3] = { optPiles: List[Pile] =>
    InRetrieveConvert.convert.result(optPiles) match {
      case Left(e) =>
        e.printStackTrace()
        throw e
      case Right(model) =>
        db.run(model.apply(binds)).map(_.effectRows).recover {
          case e: java.util.NoSuchElementException =>
            0
        }.flatMap { effectRows =>
          if (effectRows > 0) {
            InUpdateConvert.updateGen.result(optPiles) match {
              case Left(e) =>
                e.printStackTrace()
                throw e
              case Right(t) =>
                t._1.apply(binds).flatMap(s => db.run(s))
            }
          } else {
            InCreateConvert.createGen.result(optPiles) match {
              case Left(e) =>
                e.printStackTrace()
                throw e
              case Right(t) =>
                db.run(t.apply(binds))
            }
          }
        }
    }
  }

  def json2SlickRetrieveOperation(binds: List[(Any, SlickQueryBindImpl)])(
    implicit
    slickProfile: JdbcProfile,
    ec: ExecutionContext
  ): List[Pile] => Map[String, Json] => DBIO[(Map[String, QueryJsonInfo], Map[String, Json])] =
    { optPiles: List[Pile] =>
      { data: Map[String, Json] =>
        JsonOperation.unfullreadGen.flatMap(InRetrieveConvert.convert) { (jsonReader, slickWriterGen) =>
          slickWriterGen(jsonReader.apply(data))
        }.flatMap(StaticManyOperation.updateGen) { (execInfoDBIO, staticManyReader) =>
          execInfoDBIO.apply(binds).flatMap { execInfo =>
            val rowData = execInfo.columns.sortBy(_.index).map(s => s.data)
            for {
              staticMany <- DBIO.from(staticManyReader(rowData))
            } yield staticMany -> rowData
          }
        }.flatMap(JsonOperation.writeGen) { (statManyWithDataDBIO, jsonWriter) =>
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
  ): List[Pile] => Map[String, Json] => Future[Map[String, QueryJsonInfo]] =
    { optPiles: List[Pile] =>
      { data: Map[String, Json] =>
        JsonOperation.readGen.flatMap(StaticManyOperation.updateGen) { (jsonReader, staticMayGen) =>
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