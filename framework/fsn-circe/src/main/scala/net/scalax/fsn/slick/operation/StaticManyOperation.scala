package net.scalax.fsn.slick.operation

import net.scalax.fsn.core._
import net.scalax.fsn.common.atomic.FProperty
import net.scalax.fsn.slick.atomic.StaticMany
import net.scalax.fsn.slick.model.{ QueryJsonInfo, StaticManyUbw }
import shapeless._

import scala.concurrent.{ ExecutionContext, Future }

object StaticManyOperation {

  //TODO change option selector to list selector
  def updateGen(implicit ec: ExecutionContext): FPileSyntax.PileGen[Option, Future[Map[String, QueryJsonInfo]]] = {
    FPile.transformTreeList {
      new FAtomicQuery(_) {
        val aa = withRep(needAtomicOpt[StaticMany] :: FANil)
          .mapToOption {
            case (staticMayOpt :: HNil, data) =>
              Future.sequence(staticMayOpt.toList.map { eachStatic =>
                eachStatic.staticMany.map {
                  _.map { eachMany =>
                    eachMany.proName -> eachMany.gen(data.get)
                  }
                }
              }).map(_.flatten.toMap)
          }
      }.aa
    } { staticManyList =>
      Future.sequence(staticManyList).map(_.foldLeft(Map.empty[String, QueryJsonInfo]) { (font, back) =>
        font ++ back
      })
    }
  }

  /*def convert2Query(columns: FColumn)(implicit ec: ExecutionContext): Future[Map[String, QueryJsonInfo]] = {
    val staticManyCol = FColumn.filter(columns)({ case s: StaticMany[columns.DataType] => s })
    Future.sequence(staticManyCol.map { eachStatic =>
      eachStatic.staticMany.map { _.map { eachMany =>
        eachMany.proName -> eachMany.gen(columns.data.get)
      } }
    }).map(_.flatten.toMap)
  }

  def convertList2Query(columns: List[FColumn])(implicit ec: ExecutionContext): Future[Map[String, QueryJsonInfo]] = {
    Future.sequence(columns.map { eachColumn =>
      convert2Query(eachColumn)
    }).map(_.foldLeft(Map.empty[String, QueryJsonInfo]) { (font, back) =>
      font ++ back
    })
  }*/

  /*def convert2Ubw(columns: FColumn)(implicit ec: ExecutionContext): Future[List[StaticManyUbw]] = {
    val staticManyCol = FColumn.filter(columns)({ case s: StaticMany[columns.DataType] => s })
    val property = FColumn.find(columns)({ case s: FProperty[columns.DataType] => s })
    Future.sequence(
      staticManyCol
        .map(_.staticMany.map(s => s.map(t => StaticManyUbw(t.proName, property.proName, t.slaveryIdField, t.ubwGen))))
    ).map(_.flatten)
  }

  def convertList2Ubw(columns: List[FColumn])(implicit ec: ExecutionContext): Future[List[StaticManyUbw]] = {
    Future.sequence(columns.map { eachColumn =>
      convert2Ubw(eachColumn)
    }).map(_.flatten)
  }*/

  //TODO change option selector to list selector
  def ubwStaticManyGen(implicit ec: ExecutionContext): FPileSyntaxWithoutData.PileGen[Option, Future[List[StaticManyUbw]]] = {
    FPile.transformTreeListWithoutData {
      new FAtomicQuery(_) {
        val aa = withRep(needAtomicOpt[StaticMany] :: needAtomic[FProperty] :: FANil)
          .mapToOptionWithoutData {
            case (staticManyCol :: property :: HNil) =>
              Future.sequence(
                staticManyCol
                .map(_.staticMany.map(s => s.map(t => StaticManyUbw(t.proName, property.proName, t.slaveryIdField, t.ubwGen)))).toList
              ).map(_.flatten)
          }
      }.aa
    } { staticManyList =>
      /*staticManyList.map(_.map(_.foldLeft(List.empty[StaticManyUbw]) { (font, back) =>
        font ++: back
      }))*/
      Future.sequence(staticManyList).map(_.flatten)
    }
  }

}