package net.scalax.fsn.mix.helpers

import io.circe.Json
import net.scalax.fsn.core._
import net.scalax.fsn.mix.operation.PropertiesOperation
import net.scalax.fsn.mix.slickbase.{ FQueryWrap, PileListQueryWrap }
import net.scalax.fsn.slick.model._
import net.scalax.fsn.slick.operation.GroupResult
import slick.jdbc.JdbcProfile

import scala.concurrent.{ ExecutionContext, Future }

trait Slick2JsonFsnImplicit extends PilesGenHelper {

  implicit class slick2jsonExtraClass1111(listQueryWrap: PileListQueryWrap) {

    def groupResult(defaultOrders: GroupParam)(
      implicit
      slickProfile: JdbcProfile,
      ec: ExecutionContext
    ): GroupResult[Map[String, Json]] = {
      lazy val outJsonGen = PropertiesOperation.slick2jsonGroupOperation(listQueryWrap.listQueryBind).apply(listQueryWrap.columns).apply(defaultOrders)
      outJsonGen
    }

    def strResult(
      implicit
      slickProfile: JdbcProfile,
      ec: ExecutionContext
    ): JsonOut = {
      lazy val outJsonGen = PropertiesOperation.strSlick2jsonOperation(listQueryWrap.listQueryBind, listQueryWrap.slickParam).apply(listQueryWrap.columns)
      outJsonGen
    }

    /*def strResult(orderColumn: String, isDesc: Boolean = true)(
      implicit
      slickProfile: JdbcProfile,
      ec: ExecutionContext
    ): JsonOut = {
      strResult(List(ColumnOrder(orderColumn, isDesc)))
    }

    def strResult(
      implicit
      slickProfile: JdbcProfile,
      ec: ExecutionContext
    ): JsonOut = {
      strResult(Nil)
    }*/

    def filterResult(filter: Map[String, Json])(
      implicit
      slickProfile: JdbcProfile,
      ec: ExecutionContext
    ): JsonOut = {
      PropertiesOperation.filterStrSlick2jsonOperation(listQueryWrap.listQueryBind, listQueryWrap.slickParam).apply(listQueryWrap.columns).apply(filter)
    }

    /*def result(
      implicit
      slickProfile: JdbcProfile,
      ec: ExecutionContext
    ): JsonOut = {
      strResult(defaultOrders)
    }

    def result(orderColumn: String, isDesc: Boolean = true)(
      implicit
      slickProfile: JdbcProfile,
      ec: ExecutionContext
    ): JsonOut = {
      result(List(ColumnOrder(orderColumn, isDesc)))
    }

    def result(
      implicit
      slickProfile: JdbcProfile,
      ec: ExecutionContext
    ): JsonOut = {
      result(Nil)
    }*/

    def jpResult(
      implicit
      slickProfile: JdbcProfile,
      ec: ExecutionContext
    ): (() => JsonOut, () => PoiOut) = {
      lazy val outJsonGen = PropertiesOperation.strSlick2jsonOperation(listQueryWrap.listQueryBind, listQueryWrap.slickParam).apply(listQueryWrap.columns)

      //lazy val outPoiGen = PropertiesOperation.slick2PoiOperation(listQueryWrap.listQueryBind).apply(listQueryWrap.columns)

      (() => outJsonGen) -> (() => ???)
    }
    /*def jpResult(orderColumn: String, isDesc: Boolean = true)(
      implicit
      slickProfile: JdbcProfile,
      ec: ExecutionContext
    ): (() => JsonOut, () => PoiOut) = {
      jpResult(List(ColumnOrder(orderColumn, isDesc)))
    }

    def jpResult(
      implicit
      slickProfile: JdbcProfile,
      ec: ExecutionContext
    ): (() => JsonOut, () => PoiOut) = {
      jpResult(Nil)
    }*/
  }

}

trait Slick2CrudFsnImplicit extends Slick2JsonFsnImplicit {

  implicit class slick2crudExtraClass(crudQueryWrap: FQueryWrap) {
    val columns = crudQueryWrap.columns

    def result(defaultOrders: List[ColumnOrder])(
      implicit
      slickProfile: JdbcProfile,
      ec: ExecutionContext
    ): RWInfo = {
      RWInfo(
        retrieveGen = { v: Map[String, Json] =>
        val retrieveDBIO = PropertiesOperation.json2SlickRetrieveOperation(crudQueryWrap.binds).apply(columns).apply(v)
        /*for {
          (statcMany, jsonData) <- retrieveDBIO
        } yield {
          StaticManyInfo(Nil /*properties*/ , jsonData, statcMany)
        }*/
        retrieveDBIO
      },
        insertGen = { v: Map[String, Json] =>
        val createInfoDBIO = PropertiesOperation.json2SlickCreateOperation(crudQueryWrap.binds).apply(columns).apply(v)

        /*val createAction = for {
          updateStaticManyInfo <- createInfoDBIO
        } yield {
          updateStaticManyInfo
        }*/
        Future.successful(Right(createInfoDBIO))
      },
        deleteGen = (v: Map[String, Json]) => {
        PropertiesOperation.json2SlickDeleteOperation(crudQueryWrap.binds).apply(columns).apply(v)
      },
        updateGen = (v: Map[String, Json]) => {
        PropertiesOperation.json2SlickUpdateOperation(crudQueryWrap.binds).apply(columns).apply(v)
      } //,
      //staticMany = Future successful Nil //TODO StaticManyOperation.ubwStaticManyGen.result(columns).right.get
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