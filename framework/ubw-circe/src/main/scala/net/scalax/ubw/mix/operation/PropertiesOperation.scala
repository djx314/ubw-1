package net.scalax.ubw.mix.operation

import net.scalax.ubw.core._
import net.scalax.ubw.slick.model._
import net.scalax.ubw.slick.helpers.SlickQueryBindImpl
import net.scalax.ubw.slick.operation._
import net.scalax.ubw.json.operation.{ JsonOperation, ValidatorOperation }
import slick.jdbc.JdbcProfile
import io.circe.Json
import net.scalax.ubw.extraction.model.ExtractContent
import net.scalax.ubw.extraction.operation.ExtractorOperation
import net.scalax.ubw.validate.atomic.ErrorMessage
import slick.basic.BasicBackend
import slick.dbio._

import scala.concurrent.Future
import scala.concurrent.ExecutionContext

object PropertiesOperation extends PilesGenHelper {

  def filterStrSlick2jsonOperation(wQuery: SlickQueryBindImpl, slickParam: SlickParam)(
    implicit
    slickProfile: JdbcProfile,
    ec: ExecutionContext
  ): List[Pile] => Map[String, Json] => JsonOut = { optPiles: List[Pile] =>
    val jsonFilter = JsonOperation.unfullreadGen.next3333(StrOutSelectConvert.ubwGen(wQuery, slickParam)).next(JsonOperation.unSafewriteGen)
    jsonFilter.result(optPiles) match {
      case Right(slickOperation) =>
        { data: Map[String, Json] =>
          JsonOut(slickOperation(data))
        }
      case Left(e) => throw e
    }
  }

  def strSlick2jsonOperation(wQuery: SlickQueryBindImpl, slickParam: SlickParam)(
    implicit
    slickProfile: JdbcProfile,
    ec: ExecutionContext
  ): List[Pile] => JsonOut = { optPiles: List[Pile] =>
    val jsonGen = StrOutSelectConvert.ubwGen(wQuery, slickParam).next(JsonOperation.unSafewriteGen)

    jsonGen.result(optPiles) match {
      case Right(data) =>
        JsonOut(data)
      case Left(e1) => throw e1

    }
  }

  def slick2jsonGroupOperation(wQuery: SlickQueryBindImpl)(
    implicit
    slickProfile: JdbcProfile,
    ec: ExecutionContext
  ): List[Pile] => GroupParam => GroupResult[Map[String, Json]] = { optPiles: List[Pile] =>
    /*val jsonGen: PileSyntax.PileGen[GroupParam => ListAnyWrap3333[Map[String, Json]]] = GroupSelectConvert.ubwGen(wQuery).flatMap(JsonOperation.unSafewriteGen) { (slickQuery, jsonGen) =>
      { slickParam: GroupParam =>
        val result = slickQuery.result(slickParam)
        ListAnyWrap3333(result.action.map {
          case dataList =>
            ListAnyCollection3333(dataList.map(s => jsonGen(s)), None)
        }, result.statements)
      }
    }*/

    GroupSelectConvert.ubwGen(wQuery).next(JsonOperation.unSafewriteGen).result(optPiles) match {
      case Left(e) => throw e
      case Right(s) => s
    }
  }

  /*def slick2PoiOperation(wQuery: SlickQueryBindImpl)(
    implicit
    slickProfile: JdbcProfile,
    ec: ExecutionContext
  ): List[Pile] => PoiOut = { optPiles: List[Pile] =>
    val poiGen /*: PileSyntax.PileGen[Option, SlickParam => DBIO[(List[Map[String, Json]], Int)]]*/ = StrOutSelectConvert.ubwGen(wQuery).flatMap(ExcelOperation.writeGen) { (slickQuery, poiGen) =>
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
  ): List[Pile] => Map[String, Json] => Future[Either[List[ErrorMessage], DBIO[ExecInfo3[ExtractContent]]]] =
    { optPiles: List[Pile] =>
      val updateAction = JsonOperation.unfullreadGen
        .next3333[(Future[List[ErrorMessage]], InUpdateConvert.UpdateType[DataPileContent]), ({ type L[K] = (Future[List[ErrorMessage]], InUpdateConvert.UpdateType[K]) })#L](InUpdateConvert.updateGen.withFilter(ValidatorOperation.readValidator))
        .afterResult(ExtractorOperation.extractor)

      updateAction.result(optPiles) match {
        case Left(e) =>
          throw e
          e.printStackTrace
          throw e
        case Right(result) =>
          { data: Map[String, Json] =>
            val (jsonValidate, updateGen) = result(data)
            val errorMsgsF: Future[List[ErrorMessage]] = jsonValidate
            errorMsgsF.map { errorMsgs =>
              if (errorMsgs.isEmpty) {
                Right(updateGen(binds))
              } else {
                Left(errorMsgs)
              }
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
      JsonOperation.unfullreadGen.next3333(InRetrieveConvert.convert).next3333(InDeleteConvert.convert).result(optPiles) match {
        case Left(e: Exception) =>
          e.printStackTrace()
          throw e
        case Right(result) => { data: Map[String, Json] =>
          result(data)(binds).flatMap(action => action.columns(binds).map(_.effectRows))
        }
      }
    }

  def json2SlickCreateOperation(binds: List[(Any, SlickQueryBindImpl)])(
    implicit
    slickProfile: JdbcProfile,
    ec: ExecutionContext
  ): List[Pile] => Map[String, Json] => Future[Either[List[ErrorMessage], DBIO[ExecInfo3[ExtractContent]]]] = { optPiles: List[Pile] =>
    val createGen = JsonOperation.unfullreadGen
      .next3333[(Future[List[ErrorMessage]], InUpdateConvert.UpdateType[DataPileContent]), ({ type L[K] = (Future[List[ErrorMessage]], InUpdateConvert.UpdateType[K]) })#L](InCreateConvert.createGen.withFilter(ValidatorOperation.readValidator))
      .afterResult(ExtractorOperation.extractor)

    createGen.result(optPiles) match {
      case Left(e) =>
        e.printStackTrace
        throw e
      case Right(result) => { data: Map[String, Json] =>
        val (jsonValidate, updateGen) = result(data)
        val errorMsgsF: Future[List[ErrorMessage]] = jsonValidate
        errorMsgsF.map { errorMsgs =>
          if (errorMsgs.isEmpty) {
            Right(updateGen(binds))
          } else {
            Left(errorMsgs)
          }
        }
      }
    }
  }

  def slick_retrieve_i_slick_update_or_insert_o(binds: List[(Any, SlickQueryBindImpl)], db: BasicBackend#Database)(
    implicit
    ec: ExecutionContext,
    slickProfile: JdbcProfile
  ): List[Pile] => Future[ExecInfo3[List[DataWithIndex]]] = { optPiles: List[Pile] =>
    //TODO
    ???
    /*InRetrieveConvert.convert.result(optPiles) match {
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
    }*/
  }

  def json2SlickRetrieveOperation(binds: List[(Any, SlickQueryBindImpl)])(
    implicit
    slickProfile: JdbcProfile,
    ec: ExecutionContext
  ): List[Pile] => Map[String, Json] => DBIO[Map[String, Json]] =
    { optPiles: List[Pile] =>
      val retrieveAction = JsonOperation.unfullreadGen.next2222(InRetrieveConvert.convert).next(JsonOperation.unSafewriteGen)
      retrieveAction.result(optPiles) match {
        case Left(e: Exception) =>
          e.printStackTrace()
          throw e
        case Right(s) =>
          { data: Map[String, Json] =>
            s(data)(binds).map(_.columns)
          }
        /*JsonOperation.unfullreadGen.next(InRetrieveConvert.convert) { (jsonReader, slickWriterGen) =>
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
        }*/
      }
    }

  /*def staticManyOperation(
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
    }*/

}