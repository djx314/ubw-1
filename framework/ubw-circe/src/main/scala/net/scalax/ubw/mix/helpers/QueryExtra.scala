package net.scalax.fsn.mix.helpers

import io.circe.Json
import net.scalax.fsn.core._
import net.scalax.fsn.mix.operation.PropertiesOperation
import net.scalax.fsn.mix.slickbase.{ FQueryWrap, PileListQueryWrap }
import net.scalax.fsn.slick.model._
import net.scalax.fsn.slick.operation._
import slick.ast.BaseTypedType
import slick.basic.BasicProfile
import slick.dbio._
import slick.jdbc.{ JdbcActionComponent, JdbcProfile }
import slick.lifted.{ Query, Rep }
import slick.relational.RelationalProfile

import scala.concurrent.{ ExecutionContext, Future }

trait Slick2JsonFsnImplicit extends PilesGenHelper {

  implicit class slick2jsonExtraClass1111(listQueryWrap: PileListQueryWrap) {

    def groupResult(defaultOrders: GroupParam)(
      implicit
      slickProfile: JdbcProfile,
      //repToDBIO: Rep[Int] => JdbcActionComponent#QueryActionExtensionMethods[Int, NoStream],
      ec: ExecutionContext
    ): ResultWrap = {
      lazy val outJsonGen = PropertiesOperation.slick2jsonGroupOperation(listQueryWrap.listQueryBind).apply(listQueryWrap.columns).apply(defaultOrders)
      outJsonGen
    }

    def strResult(defaultOrders: List[ColumnOrder])(
      implicit
      slickProfile: JdbcProfile,
      ec: ExecutionContext
    ): JsonOut = {
      lazy val outJsonGen = PropertiesOperation.strSlick2jsonOperation(listQueryWrap.listQueryBind, defaultOrders).apply(listQueryWrap.columns)
      outJsonGen
    }

    def strResult(orderColumn: String, isDesc: Boolean = true)(
      implicit
      //jsonEv: Query[_, List[Any], List] => JdbcActionComponent#StreamingQueryActionExtensionMethods[List[List[Any]], List[Any]],
      //repToDBIO: Rep[Int] => JdbcActionComponent#QueryActionExtensionMethods[Int, NoStream],
      slickProfile: JdbcProfile,
      ec: ExecutionContext
    ): JsonOut = {
      strResult(List(ColumnOrder(orderColumn, isDesc)))
    }

    def strResult(
      implicit
      //jsonEv: Query[_, List[Any], List] => JdbcActionComponent#StreamingQueryActionExtensionMethods[List[List[Any]], List[Any]],
      //repToDBIO: Rep[Int] => JdbcActionComponent#QueryActionExtensionMethods[Int, NoStream],
      slickProfile: JdbcProfile,
      ec: ExecutionContext
    ): JsonOut = {
      strResult(Nil)
    }

    def filterResult(
      implicit
      //jsonEv: Query[_, List[Any], List] => JdbcActionComponent#StreamingQueryActionExtensionMethods[List[List[Any]], List[Any]],
      //repToDBIO: Rep[Int] => JdbcActionComponent#QueryActionExtensionMethods[Int, NoStream],
      slickProfile: JdbcProfile,
      ec: ExecutionContext
    ): JsonOut = {
      PropertiesOperation.filterStrSlick2jsonOperation(listQueryWrap.listQueryBind, Nil).apply(listQueryWrap.columns)
    }

    def result(defaultOrders: List[ColumnOrder])(
      implicit
      //jsonEv: Query[_, List[Any], List] => JdbcActionComponent#StreamingQueryActionExtensionMethods[List[List[Any]], List[Any]],
      //repToDBIO: Rep[Int] => JdbcActionComponent#QueryActionExtensionMethods[Int, NoStream],
      slickProfile: JdbcProfile,
      ec: ExecutionContext
    ): JsonOut = {
      /*lazy val outJsonGen = PropertiesOperation.slick2jsonOperation(listQueryWrap.listQueryBind).apply(listQueryWrap.columns)
      outJsonGen*/
      strResult(defaultOrders)
    }

    def result(orderColumn: String, isDesc: Boolean = true)(
      implicit
      //jsonEv: Query[_, List[Any], List] => JdbcActionComponent#StreamingQueryActionExtensionMethods[List[List[Any]], List[Any]],
      //repToDBIO: Rep[Int] => JdbcActionComponent#QueryActionExtensionMethods[Int, NoStream],
      slickProfile: JdbcProfile,
      ec: ExecutionContext
    ): JsonOut = {
      result(List(ColumnOrder(orderColumn, isDesc)))
    }

    def result(
      implicit
      //jsonEv: Query[_, List[Any], List] => JdbcActionComponent#StreamingQueryActionExtensionMethods[List[List[Any]], List[Any]],
      //repToDBIO: Rep[Int] => JdbcActionComponent#QueryActionExtensionMethods[Int, NoStream],
      slickProfile: JdbcProfile,
      ec: ExecutionContext
    ): JsonOut = {
      result(Nil)
    }

    def jpResult(defaultOrders: List[ColumnOrder])(
      implicit
      //jsonEv: Query[_, List[Any], List] => JdbcActionComponent#StreamingQueryActionExtensionMethods[List[List[Any]], List[Any]],
      //repToDBIO: Rep[Int] => JdbcActionComponent#QueryActionExtensionMethods[Int, NoStream],
      slickProfile: JdbcProfile,
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
      //val newPiles = withExtraCols.map(col => Pile.applyOpt(AtomicPathImpl(col.cols)))
      lazy val outJsonGen = PropertiesOperation.strSlick2jsonOperation(listQueryWrap.listQueryBind, defaultOrders).apply(listQueryWrap.columns)

      lazy val outPoiGen = PropertiesOperation.slick2PoiOperation(listQueryWrap.listQueryBind).apply(listQueryWrap.columns)

      (() => outJsonGen) -> (() => outPoiGen)
    }

    def jpResult(orderColumn: String, isDesc: Boolean = true)(
      implicit
      //jsonEv: Query[_, List[Any], List] => JdbcActionComponent#StreamingQueryActionExtensionMethods[List[List[Any]], List[Any]],
      //repToDBIO: Rep[Int] => JdbcActionComponent#QueryActionExtensionMethods[Int, NoStream],
      slickProfile: JdbcProfile,
      ec: ExecutionContext
    ): (() => JsonOut, () => PoiOut) = {
      jpResult(List(ColumnOrder(orderColumn, isDesc)))
    }

    def jpResult(
      implicit
      //jsonEv: Query[_, List[Any], List] => JdbcActionComponent#StreamingQueryActionExtensionMethods[List[List[Any]], List[Any]],
      //repToDBIO: Rep[Int] => JdbcActionComponent#QueryActionExtensionMethods[Int, NoStream],
      slickProfile: JdbcProfile,
      ec: ExecutionContext
    ): (() => JsonOut, () => PoiOut) = {
      jpResult(Nil)
    }

  }

}

trait Slick2CrudFsnImplicit extends Slick2JsonFsnImplicit {

  implicit class slick2crudExtraClass(crudQueryWrap: FQueryWrap) {
    val columns = crudQueryWrap.columns
    //lazy val properties = PropertiesOperation.RWPropertiesGen(columns).right.get._2

    def result(defaultOrders: List[ColumnOrder])(
      implicit
      slickProfile: JdbcProfile,
      ec: ExecutionContext
    ): RWInfo = {
      RWInfo(
        properties = Nil, //properties,
        retrieveGen = { v: Map[String, Json] =>
        val retrieveDBIO = PropertiesOperation.json2SlickRetrieveOperation(crudQueryWrap.binds).apply(columns).apply(v)

        for {
          (statcMany, jsonData) <- retrieveDBIO
        } yield {
          StaticManyInfo(Nil /*properties*/ , jsonData, statcMany)
        }
      },
        insertGen = { v: Map[String, Json] =>
        val createInfoDBIO = PropertiesOperation.json2SlickCreateOperation(crudQueryWrap.binds).apply(columns).apply(v)

        for {
          updateStaticManyInfo <- createInfoDBIO
        } yield {
          updateStaticManyInfo
        }
      },
        deleteGen = (v: Map[String, Json]) => {
        PropertiesOperation.json2SlickDeleteOperation(crudQueryWrap.binds).apply(columns).apply(v)
      },
        updateGen = (v: Map[String, Json]) => {
        PropertiesOperation.json2SlickUpdateOperation(crudQueryWrap.binds).apply(columns).apply(v)
      },
        staticMany = Future successful Nil //TODO StaticManyOperation.ubwStaticManyGen.result(columns).right.get
      )
    }

    def result(orderColumn: String, isDesc: Boolean = true)(
      implicit
      slickProfile: JdbcProfile,
      ec: ExecutionContext
    ): RWInfo = {
      result(List(ColumnOrder(orderColumn, isDesc)))
    }

    def result(
      implicit
      slickProfile: JdbcProfile,
      ec: ExecutionContext
    ): RWInfo = {
      result(Nil)
    }
  }

}