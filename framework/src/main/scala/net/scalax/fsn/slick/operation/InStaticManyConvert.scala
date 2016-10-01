package net.scalax.fsn.slick.operation

import net.scalax.fsn.core.FColumn
import net.scalax.fsn.common.FProperty
import net.scalax.fsn.slick.atomic.StaticMany
import net.scalax.fsn.slick.model.{QueryJsonInfo, StaticManyUbw}

import scala.concurrent.{ExecutionContext, Future}

object InStaticManyConvert {

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