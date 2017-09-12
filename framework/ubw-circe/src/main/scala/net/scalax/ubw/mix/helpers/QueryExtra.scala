package net.scalax.ubw.mix.helpers

import io.circe.Json
import net.scalax.ubw.core._
import net.scalax.ubw.mix.operation.PropertiesOperation
import net.scalax.ubw.mix.slickbase.{ FQueryWrap, PileListQueryWrap }
import net.scalax.ubw.slick.model._
import net.scalax.ubw.slick.operation.GroupResult
import slick.jdbc.JdbcProfile

import scala.concurrent.ExecutionContext

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

    def filterResult(filter: Map[String, Json])(
      implicit
      slickProfile: JdbcProfile,
      ec: ExecutionContext
    ): JsonOut = {
      PropertiesOperation.filterStrSlick2jsonOperation(listQueryWrap.listQueryBind, listQueryWrap.slickParam).apply(listQueryWrap.columns).apply(filter)
    }

    def jpResult(
      implicit
      slickProfile: JdbcProfile,
      ec: ExecutionContext
    ): (() => JsonOut, () => PoiOut) = {
      lazy val outJsonGen = PropertiesOperation.strSlick2jsonOperation(listQueryWrap.listQueryBind, listQueryWrap.slickParam).apply(listQueryWrap.columns)

      //lazy val outPoiGen = PropertiesOperation.slick2PoiOperation(listQueryWrap.listQueryBind).apply(listQueryWrap.columns)

      (() => outJsonGen) -> (() => ???)
    }

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
      new RWInfo {
        override lazy val retrieveGen = {
          val retrieveDBIO = PropertiesOperation.json2SlickRetrieveOperation(crudQueryWrap.binds).apply(columns)
          retrieveDBIO
        }

        override lazy val insertGen = {
          val createInfoDBIO = PropertiesOperation.json2SlickCreateOperation(crudQueryWrap.binds).apply(columns)
          createInfoDBIO
        }

        override lazy val deleteGen = {
          PropertiesOperation.json2SlickDeleteOperation(crudQueryWrap.binds).apply(columns)
        }

        override lazy val updateGen = {
          PropertiesOperation.json2SlickUpdateOperation(crudQueryWrap.binds).apply(columns)
        }
      }
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