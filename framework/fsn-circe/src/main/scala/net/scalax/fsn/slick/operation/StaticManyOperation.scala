package net.scalax.fsn.slick.operation

import net.scalax.fsn.core._
import net.scalax.fsn.common.atomic.FProperty
import net.scalax.fsn.slick.atomic.{OneToOneUpdate, SlickUpdate, StaticMany}
import net.scalax.fsn.slick.helpers.SlickQueryBindImpl
import net.scalax.fsn.slick.model.{QueryJsonInfo, StaticManyUbw, UpdateStaticManyInfo}
import net.scalax.fsn.slick.operation.InUpdateConvert2.{needAtomic, needAtomicOpt}
import slick.dbio.DBIO
import slick.jdbc.JdbcActionComponent
import slick.lifted.Query
import shapeless._

import scala.concurrent.{ExecutionContext, Future}

object StaticManyOperation extends FAtomicGenHelper with FAtomicShapeHelper {

  def updateGen(implicit ec: ExecutionContext): FPileSyntax.PileGen[Option, Future[Map[String, QueryJsonInfo]]] = {
    FPile.transformTreeList { path =>
      FAtomicQuery(needAtomicOpt[StaticMany] :: HNil)
        .mapToOption(path) { case (staticMayOpt :: HNil, data) =>
          Future.sequence(staticMayOpt.toList.map { eachStatic =>
            eachStatic.staticMany.map { _.map { eachMany =>
              eachMany.proName -> eachMany.gen(data.get)
            } }
          }).map(_.flatten.toMap)
        }
    } { staticManyList =>
      Future.sequence(staticManyList).map(_.foldLeft(Map.empty[String, QueryJsonInfo]) { (font, back) =>
        font ++ back
      })
    }
  }

  def convert2Query(columns: FColumn)(implicit ec: ExecutionContext): Future[Map[String, QueryJsonInfo]] = {
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
  }

  def convert2Ubw(columns: FColumn)(implicit ec: ExecutionContext): Future[List[StaticManyUbw]] = {
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
  }

}