package net.scalax.fsn.mix.operation

import net.scalax.fsn.common.atomic.{ DefaultValue, FDescribe, FProperty }
import net.scalax.fsn.core._
import net.scalax.fsn.json.atomic.JsonWriter
import net.scalax.fsn.slick.atomic._
import net.scalax.fsn.slick.model._
import net.scalax.fsn.slick.helpers.{ SlickQueryBindImpl, TypeHelpers }
import net.scalax.fsn.slick.operation._
import net.scalax.fsn.json.operation._
import net.scalax.fsn.excel.atomic.PoiWriter
import slick.jdbc.JdbcActionComponent
import shapeless._
import io.circe.Json
import slick.ast.{ BaseTypedType, Ordering }
import slick.dbio._
import slick.lifted._
import slick.relational.RelationalProfile

import scala.concurrent.Future
import scala.concurrent.ExecutionContext

object InAndOutOperation extends FPilesGenHelper with FAtomicValueHelper {

  def futureGen(implicit ec: ExecutionContext): FPileSyntax.PileGen[Future[List[FAtomicValue]]] = {
    FPile.transformTreeList {
      new FAtomicQuery(_) {
        val aa = withRep(needAtomic[SlickCreate])
          .mapTo {
            case (_, data) =>
              (data match {
                case dataWrap @ FSomeValue(_) =>
                  Future successful dataWrap
                case FFValue(futureData) =>
                  futureData.map(set)
              }): Future[FAtomicValue]
          }
      }.aa
    } { genList =>
      Future.sequence(genList)
    }
  }

}