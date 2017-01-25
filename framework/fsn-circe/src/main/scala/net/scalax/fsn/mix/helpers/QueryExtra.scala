package net.scalax.fsn.mix.helpers

import io.circe.Json
import net.scalax.fsn.core._
import net.scalax.fsn.json.operation.{ExcelOperation, JsonOperation}
import net.scalax.fsn.mix.operation.PropertiesOperation
import net.scalax.fsn.mix.slickbase.{FQueryWrap, ListQueryWrap, PileListQueryWrap}
import net.scalax.fsn.slick.atomic.{AutoInc, SlickRetrieve}
import net.scalax.fsn.slick.model._
import net.scalax.fsn.slick.operation._
import slick.basic.BasicProfile
import slick.dbio._
import slick.jdbc.JdbcActionComponent
import slick.lifted.{FlatShapeLevel, Query, Rep, Shape}
import slick.relational.RelationalProfile

import scala.concurrent.ExecutionContext
import scala.reflect.runtime.universe._
import scala.language.implicitConversions

trait Slick2JsonFsnImplicit extends FPilesGenHelper {

  implicit class slick2jsonExtraClass(listQueryWrap: ListQueryWrap) {
    def result(defaultOrders: List[ColumnOrder])
                  (
                    implicit
                    jsonEv: Query[_, Seq[Any], Seq] => BasicProfile#StreamingQueryActionExtensionMethods[Seq[Seq[Any]], Seq[Any]],
                    repToDBIO: Rep[Int] => BasicProfile#QueryActionExtensionMethods[Int, NoStream],
                    ec: ExecutionContext
                  ): JsonOut = {
      lazy val withExtraCols = OutSelectConvert.extraSubCol(listQueryWrap.columns)
      /*lazy val queryWrap: JsonQuery = SelectOperation.encode(withExtraCols, listQueryWrap.listQueryBind)

      val gen = { slickParam: SlickParam =>
        queryWrap.jsonResult(defaultOrders).apply(slickParam).map { result =>
          result._1.map(JsonOperation.writeJ) -> result._2
        }
      }*/

      val newPiles = withExtraCols.map(col => FPile.applyOpt(FPathImpl(col.cols)))
      lazy val outJsonGen = PropertiesOperation.slick2jsonOperation(listQueryWrap.listQueryBind).apply(newPiles)
      outJsonGen
      //JsonOut(withExtraCols.map(PropertiesOperation.convertProperty), gen)
    }

    def result(orderColumn: String, isDesc: Boolean = true)
                  (
                    implicit
                    jsonEv: Query[_, Seq[Any], Seq] => BasicProfile#StreamingQueryActionExtensionMethods[Seq[Seq[Any]], Seq[Any]],
                    repToDBIO: Rep[Int] => BasicProfile#QueryActionExtensionMethods[Int, NoStream],
                    ec: ExecutionContext
                  ): JsonOut = {
      result(List(ColumnOrder(orderColumn, isDesc)))
    }

    def result
              (
                implicit
                jsonEv: Query[_, Seq[Any], Seq] => BasicProfile#StreamingQueryActionExtensionMethods[Seq[Seq[Any]], Seq[Any]],
                repToDBIO: Rep[Int] => BasicProfile#QueryActionExtensionMethods[Int, NoStream],
                ec: ExecutionContext
              ): JsonOut = {
      result(Nil)
    }

    def jpResult(defaultOrders: List[ColumnOrder])
              (
                implicit
                jsonEv: Query[_, Seq[Any], Seq] => BasicProfile#StreamingQueryActionExtensionMethods[Seq[Seq[Any]], Seq[Any]],
                repToDBIO: Rep[Int] => BasicProfile#QueryActionExtensionMethods[Int, NoStream],
                ec: ExecutionContext
              ): (() => JsonOut, () => PoiOut) = {
      lazy val withExtraCols = OutSelectConvert.extraSubCol(listQueryWrap.columns)
      /*lazy val queryWrap: JsonQuery = SelectOperation.encode(withExtraCols, listQueryWrap.listQueryBind)

      val jsonGen = { slickParam: SlickParam =>
        queryWrap.jsonResult(defaultOrders).apply(slickParam).map { result =>
          result._1.map(JsonOperation.writeJ) -> result._2
        }
      }

      val poiGen = { slickParam: SlickParam =>
        queryWrap.jsonResult(defaultOrders).apply(slickParam).map { result =>
          //TODO 下面的实现补上 InRetrieveOperation.filterInRetrieve
          result._1.map(InRetrieveOperation.filterInRetrieve).map(ExcelOperation.writeP) -> result._2
        }
      }

      JsonOut(withExtraCols.map(PropertiesOperation.convertProperty), jsonGen) ->
      PoiOut(withExtraCols.map(PropertiesOperation.convertProperty), poiGen)*/

      //==========================================================================================
      val newPiles = withExtraCols.map(col => FPile.applyOpt(FPathImpl(col.cols)))
      /*val outQueryWrap = OutSelectConvert.ubwGen(listQueryWrap.listQueryBind)
      val outJsonGen = outQueryWrap.flatMap(JsonOperation.writeGen) { (slickDBIO, jsonGen) =>
        { slickParam: SlickParam =>
          slickDBIO.slickResult(defaultOrders).apply(slickParam).map { result =>
            result._1.map(s => jsonGen(s)) -> result._2
          }
        }
      }
      val outPoiGen = outQueryWrap.flatMap(ExcelOperation.writeGen) { (slickDBIO, poiGen) =>
        { slickParam: SlickParam =>
          slickDBIO.slickResult(defaultOrders).apply(slickParam).map { result =>
            result._1.map(s => poiGen(s)) -> result._2
          }
        }
      }
      (() => JsonOut(withExtraCols.map(PropertiesOperation.convertProperty), outJsonGen.result(newPiles) match {
        case Left(e) => throw e
        case Right(s) => s
      })) ->
        (() => PoiOut(withExtraCols.map(PropertiesOperation.convertProperty), outPoiGen.result(newPiles) match {
          case Left(e) => throw e
          case Right(s) => s
        }))*/
      lazy val outJsonGen = PropertiesOperation.slick2jsonOperation(listQueryWrap.listQueryBind).apply(newPiles)
      lazy val outPoiGen = PropertiesOperation.slick2PoiOperation(listQueryWrap.listQueryBind).apply(newPiles)
      (() => outJsonGen) -> (() => outPoiGen)
    }

    def jpResult(orderColumn: String, isDesc: Boolean = true)
              (
                implicit
                jsonEv: Query[_, Seq[Any], Seq] => BasicProfile#StreamingQueryActionExtensionMethods[Seq[Seq[Any]], Seq[Any]],
                repToDBIO: Rep[Int] => BasicProfile#QueryActionExtensionMethods[Int, NoStream],
                ec: ExecutionContext
              ): (() => JsonOut, () => PoiOut) = {
      jpResult(List(ColumnOrder(orderColumn, isDesc)))
    }

    def jpResult
    (
      implicit
      jsonEv: Query[_, Seq[Any], Seq] => BasicProfile#StreamingQueryActionExtensionMethods[Seq[Seq[Any]], Seq[Any]],
      repToDBIO: Rep[Int] => BasicProfile#QueryActionExtensionMethods[Int, NoStream],
      ec: ExecutionContext
    ): (() => JsonOut, () => PoiOut) = {
      jpResult(Nil)
    }

  }

  implicit class slick2jsonExtraClass1111(listQueryWrap: PileListQueryWrap) {
    def result(defaultOrders: List[ColumnOrder])
              (
                implicit
                jsonEv: Query[_, Seq[Any], Seq] => BasicProfile#StreamingQueryActionExtensionMethods[Seq[Seq[Any]], Seq[Any]],
                repToDBIO: Rep[Int] => BasicProfile#QueryActionExtensionMethods[Int, NoStream],
                ec: ExecutionContext
              ): JsonOut = {
      lazy val outJsonGen = PropertiesOperation.slick2jsonOperation(listQueryWrap.listQueryBind).apply(listQueryWrap.columns)
      outJsonGen
    }

    def result(orderColumn: String, isDesc: Boolean = true)
              (
                implicit
                jsonEv: Query[_, Seq[Any], Seq] => BasicProfile#StreamingQueryActionExtensionMethods[Seq[Seq[Any]], Seq[Any]],
                repToDBIO: Rep[Int] => BasicProfile#QueryActionExtensionMethods[Int, NoStream],
                ec: ExecutionContext
              ): JsonOut = {
      result(List(ColumnOrder(orderColumn, isDesc)))
    }

    def result
    (
      implicit
      jsonEv: Query[_, Seq[Any], Seq] => BasicProfile#StreamingQueryActionExtensionMethods[Seq[Seq[Any]], Seq[Any]],
      repToDBIO: Rep[Int] => BasicProfile#QueryActionExtensionMethods[Int, NoStream],
      ec: ExecutionContext
    ): JsonOut = {
      result(Nil)
    }

    def jpResult(defaultOrders: List[ColumnOrder])
                (
                  implicit
                  jsonEv: Query[_, Seq[Any], Seq] => BasicProfile#StreamingQueryActionExtensionMethods[Seq[Seq[Any]], Seq[Any]],
                  repToDBIO: Rep[Int] => BasicProfile#QueryActionExtensionMethods[Int, NoStream],
                  ec: ExecutionContext
                ): (() => JsonOut, () => PoiOut) = {
      /*lazy val withExtraCols = OutSelectConvert.extraSubCol(listQueryWrap.columns)
      lazy val queryWrap: JsonQuery = SelectOperation.encode(withExtraCols, listQueryWrap.listQueryBind)

      val jsonGen = { slickParam: SlickParam =>
        queryWrap.jsonResult(defaultOrders).apply(slickParam).map { result =>
          result._1.map(JsonOperation.writeJ) -> result._2
        }
      }

      val poiGen = { slickParam: SlickParam =>
        queryWrap.jsonResult(defaultOrders).apply(slickParam).map { result =>
          //TODO 下面的实现补上 InRetrieveOperation.filterInRetrieve
          result._1.map(InRetrieveOperation.filterInRetrieve).map(ExcelOperation.writeP) -> result._2
        }
      }

      JsonOut(withExtraCols.map(PropertiesOperation.convertProperty), jsonGen) ->
        PoiOut(withExtraCols.map(PropertiesOperation.convertProperty), poiGen)*/

      //==========================================================================================
      //val newPiles = withExtraCols.map(col => FPile.applyOpt(FPathImpl(col.cols)))
      lazy val outJsonGen = PropertiesOperation.slick2jsonOperation(listQueryWrap.listQueryBind).apply(listQueryWrap.columns)

      lazy val outPoiGen = PropertiesOperation.slick2PoiOperation(listQueryWrap.listQueryBind).apply(listQueryWrap.columns)

      (() => outJsonGen) -> (() => outPoiGen)
    }

    def jpResult(orderColumn: String, isDesc: Boolean = true)
                (
                  implicit
                  jsonEv: Query[_, Seq[Any], Seq] => BasicProfile#StreamingQueryActionExtensionMethods[Seq[Seq[Any]], Seq[Any]],
                  repToDBIO: Rep[Int] => BasicProfile#QueryActionExtensionMethods[Int, NoStream],
                  ec: ExecutionContext
                ): (() => JsonOut, () => PoiOut) = {
      jpResult(List(ColumnOrder(orderColumn, isDesc)))
    }

    def jpResult
    (
      implicit
      jsonEv: Query[_, Seq[Any], Seq] => BasicProfile#StreamingQueryActionExtensionMethods[Seq[Seq[Any]], Seq[Any]],
      repToDBIO: Rep[Int] => BasicProfile#QueryActionExtensionMethods[Int, NoStream],
      ec: ExecutionContext
    ): (() => JsonOut, () => PoiOut) = {
      jpResult(Nil)
    }

  }

}

trait Slick2CrudFsnImplicit extends Slick2JsonFsnImplicit {

  implicit class slick2crudExtraClass(crudQueryWrap: FQueryWrap) {
    val columns = crudQueryWrap.columns
    lazy val properties = PropertiesOperation.convertColumn(columns)

    def result
    (defaultOrders: List[ColumnOrder])
    (
      implicit
      jsonEv: Query[_, Seq[Any], Seq] => BasicProfile#StreamingQueryActionExtensionMethods[Seq[Seq[Any]], Seq[Any]],
      //repToDBIO: Rep[Int] => BasicProfile#QueryActionExtensionMethods[Int, NoStream],
      retrieve: Query[_, String, Seq] => BasicProfile#StreamingQueryActionExtensionMethods[Seq[String], String],
      insertConv: Query[_, Seq[Any], Seq] => JdbcActionComponent#InsertActionExtensionMethods[Seq[Any]],
      deleteConV: Query[RelationalProfile#Table[_], _, Seq] => JdbcActionComponent#DeleteActionExtensionMethods,
      updateConV: Query[_, Seq[Any], Seq] => JdbcActionComponent#UpdateActionExtensionMethods[Seq[Any]],
      ec: ExecutionContext
    ): RWInfo = {
      RWInfo(
        properties = properties,
        /*jsonGen = {
          crudQueryWrap.listQueryWrap.result(defaultOrders)
        },*/
        retrieveGen = { v: Map[String, Json] =>
          val jsonData = JsonOperation.readWithFilter(columns) { eachColumn =>
            FColumn.findOpt(eachColumn) { case s: SlickRetrieve[eachColumn.DataType] => s }.map(_.primaryGen.isDefined).getOrElse(false)
          } (v)
          val staticManyFuture = PropertiesOperation.staticManyOperation.apply(columns.map(s => FPile.applyOpt(FPathImpl(s.cols)))).apply(v)
          for {
            execInfo <- RetrieveOperation.parseInsert(crudQueryWrap.binds, jsonData)
            //staticMany = StaticManyOperation.convertList2Query(execInfo.columns)
            staticM <- DBIO.from(staticManyFuture)
          } yield {
            val jsonResult = JsonOperation.writeJ(execInfo.columns)
            StaticManyInfo(properties, jsonResult, staticM)
          }
        },
        insertGen = { v: Map[String, Json] =>
          val jsonData = JsonOperation.readWithFilter(columns){ eachColumn =>
            ! FColumn.findOpt(eachColumn) { case s: AutoInc[eachColumn.DataType] => s }.map(_.isAutoInc).getOrElse(false)
          }(v)
          val staticManyFuture = PropertiesOperation.staticManyOperation.apply(columns.map(s => FPile.applyOpt(FPathImpl(s.cols)))).apply(v)
          for {
            execInfo <- CreateOperation.parseInsert(crudQueryWrap.binds, jsonData)
            //staticMany = StaticManyOperation.convertList2Query(execInfo.columns)
            staticM <- DBIO.from(staticManyFuture)
          } yield {
            UpdateStaticManyInfo(execInfo.effectRows, staticM)
          }
        },
        deleteGen = (v: Map[String, Json]) => {
          val primaryColumns = columns.filter { col => FColumn.findOpt(col) { case retrieve: SlickRetrieve[col.DataType] => retrieve }.map(_.primaryGen.isDefined).getOrElse(false) }
          val jsonData = JsonOperation.readJ(primaryColumns)(v)
          //val staticMany = StaticManyOperation.convertList2Query(jsonData)
          val staticManyFuture = PropertiesOperation.staticManyOperation.apply(columns.map(s => FPile.applyOpt(FPathImpl(s.cols)))).apply(v)
          for {
            updateInfo <- DeleteOperation.parseInsert(crudQueryWrap.binds, jsonData)
            staticM <- DBIO.from(staticManyFuture)
          } yield {
            updateInfo.copy(many = staticM).effectRows
          }
        },
        updateGen = (v: Map[String, Json]) => {
          //val staticMany = StaticManyOperation.convertList2Query(jsonData)
          /*val jsonData = JsonOperation.readJ(columns)(v)
          val staticMany = StaticManyOperation.convertList2Query(jsonData)
          for {
            updateInfo <- UpdateOperation.parseInsert(crudQueryWrap.binds, jsonData)
            staticM <- DBIO.from(staticMany)
          } yield {
            updateInfo.copy(many = staticM)
          }*/
          val staticMany = PropertiesOperation.staticManyOperation.apply(columns.map(s => FPile.applyOpt(FPathImpl(s.cols)))).apply(v)
          val updateInfoDBIO = PropertiesOperation.json2SlickUpdateOperation(crudQueryWrap.binds).apply(columns.map(s => FPile.applyOpt(FPathImpl(s.cols)))).apply(v)
          for {
            updateInfo <- updateInfoDBIO
            staticM <- DBIO.from(staticMany)
          } yield {
            updateInfo.copy(many = staticM)
          }
        },
        staticMany = StaticManyOperation.ubwStaticManyGen.result(columns.map(s => FPile.applyOpt(FPathImpl(s.cols)))).right.get
      )
    }

    def result
    (orderColumn: String, isDesc: Boolean = true)
    (
      implicit
      jsonEv: Query[_, Seq[Any], Seq] => BasicProfile#StreamingQueryActionExtensionMethods[Seq[Seq[Any]], Seq[Any]],
      //repToDBIO: Rep[Int] => BasicProfile#QueryActionExtensionMethods[Int, NoStream],
      retrieve: Query[_, String, Seq] => BasicProfile#StreamingQueryActionExtensionMethods[Seq[String], String],
      insertConv: Query[_, Seq[Any], Seq] => JdbcActionComponent#InsertActionExtensionMethods[Seq[Any]],
      deleteConV: Query[RelationalProfile#Table[_], _, Seq] => JdbcActionComponent#DeleteActionExtensionMethods,
      updateConV: Query[_, Seq[Any], Seq] => JdbcActionComponent#UpdateActionExtensionMethods[Seq[Any]],
      ec: ExecutionContext
    ): RWInfo = {
      result(List(ColumnOrder(orderColumn, isDesc)))
    }

    def result
    (
      implicit
      jsonEv: Query[_, Seq[Any], Seq] => BasicProfile#StreamingQueryActionExtensionMethods[Seq[Seq[Any]], Seq[Any]],
      //repToDBIO: Rep[Int] => BasicProfile#QueryActionExtensionMethods[Int, NoStream],
      retrieve: Query[_, String, Seq] => BasicProfile#StreamingQueryActionExtensionMethods[Seq[String], String],
      insertConv: Query[_, Seq[Any], Seq] => JdbcActionComponent#InsertActionExtensionMethods[Seq[Any]],
      deleteConV: Query[RelationalProfile#Table[_], _, Seq] => JdbcActionComponent#DeleteActionExtensionMethods,
      updateConV: Query[_, Seq[Any], Seq] => JdbcActionComponent#UpdateActionExtensionMethods[Seq[Any]],
      ec: ExecutionContext
    ): RWInfo = {
      result(Nil)
    }
  }

}