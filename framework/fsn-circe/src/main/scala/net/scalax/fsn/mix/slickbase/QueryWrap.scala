package net.scalax.fsn.mix.slickbase

import net.scalax.fsn.core.{FColumn, FPile}
import net.scalax.fsn.slick.helpers.SlickQueryBindImpl

import scala.concurrent.ExecutionContext

case class PileListQueryWrap(
  columns: List[FPile[Option]],
  listQueryBind: SlickQueryBindImpl
)(implicit val ec: ExecutionContext) {
}

case class ListQueryWrap(
  columns: List[FColumn],
  listQueryBind: SlickQueryBindImpl
)(implicit val ec: ExecutionContext) {

  /*def result
  (defaultOrders: List[ColumnOrder])
  (
    implicit
    jsonEv: Query[_, Seq[Any], Seq] => BasicProfile#StreamingQueryActionExtensionMethods[Seq[Seq[Any]], Seq[Any]],
    repToDBIO: Rep[Int] => BasicProfile#QueryActionExtensionMethods[Int, NoStream]
  ): JsonOut = {
    val gen = { slickParam: SlickParam =>
      queryWrap.jsonResult(defaultOrders).apply(slickParam).map { result =>
        result._1.map(JsonOperation.writeJ) -> result._2
      }
    }
    JsonOut(withExtraCols.map(PropertiesOperation.convertProperty), gen)
  }

  def result
  (orderColumn: String, isDesc: Boolean = true)
  (
    implicit
    jsonEv: Query[_, Seq[Any], Seq] => BasicProfile#StreamingQueryActionExtensionMethods[Seq[Seq[Any]], Seq[Any]],
    repToDBIO: Rep[Int] => BasicProfile#QueryActionExtensionMethods[Int, NoStream]
  ): JsonOut = {
    result(List(ColumnOrder(orderColumn, isDesc)))
  }

  def result
  (
    implicit
    jsonEv: Query[_, Seq[Any], Seq] => BasicProfile#StreamingQueryActionExtensionMethods[Seq[Seq[Any]], Seq[Any]],
    repToDBIO: Rep[Int] => BasicProfile#QueryActionExtensionMethods[Int, NoStream]
  ): JsonOut = {
    result(Nil)
  }*/

}

case class QueryWrap(
  binds: List[(Any, SlickQueryBindImpl)],
  listQueryWrap: ListQueryWrap
)/*(implicit val ec: ExecutionContext)*/{

  //lazy val queryWrap: listQueryWrap.queryWrap.type = listQueryWrap.queryWrap
  /*val columns = listQueryWrap.columns
  lazy val properties = PropertiesOperation.convertColumn(columns)

  object aa extends Slick2JsonFsnImplicit
  import aa._

  def result
  (defaultOrders: List[ColumnOrder])
  (
    implicit
    jsonEv: Query[_, Seq[Any], Seq] => BasicProfile#StreamingQueryActionExtensionMethods[Seq[Seq[Any]], Seq[Any]],
    repToDBIO: Rep[Int] => BasicProfile#QueryActionExtensionMethods[Int, NoStream],
    retrieve: Query[_, String, Seq] => BasicProfile#StreamingQueryActionExtensionMethods[Seq[String], String],
    insertConv: Query[_, Seq[Any], Seq] => JdbcActionComponent#InsertActionExtensionMethods[Seq[Any]],
    deleteConV: Query[RelationalProfile#Table[_], _, Seq] => JdbcActionComponent#DeleteActionExtensionMethods,
    updateConV: Query[_, Seq[Any], Seq] => JdbcActionComponent#UpdateActionExtensionMethods[Seq[Any]],
    ec: ExecutionContext
  ): QueryJsonInfo = {
    QueryJsonInfo(
      properties = properties,
      jsonGen = {
        listQueryWrap.result(defaultOrders)
      },
      retrieveGen = { v: Map[String, Json] =>
        val jsonData = JsonOperation.readWithFilter(columns) { eachColumn =>
          FColumn.findOpt(eachColumn) { case s: SlickRetrieve[eachColumn.DataType] => s }.map(_.primaryGen.isDefined).getOrElse(false)
        } (v)
        for {
          execInfo <- RetrieveOperation.parseInsert(binds, jsonData)
          staticMany = StaticManyOperation.convertList2Query(execInfo.columns)
          staticM <- DBIO.from(staticMany)
        } yield {
          val jsonResult = JsonOperation.writeJ(execInfo.columns)
          StaticManyInfo(properties, jsonResult, staticM)
        }
      },
      insertGen = { v: Map[String, Json] =>
        val jsonData = JsonOperation.readWithFilter(columns){ eachColumn =>
          ! FColumn.findOpt(eachColumn) { case s: AutoInc[eachColumn.DataType] => s }.map(_.isAutoInc).getOrElse(false)
        }(v)
        for {
          execInfo <- CreateOperation.parseInsert(binds, jsonData)
          staticMany = StaticManyOperation.convertList2Query(execInfo.columns)
          staticM <- DBIO.from(staticMany)
        } yield {
          UpdateStaticManyInfo(execInfo.effectRows, staticM)
        }
      },
      deleteGen = (v: Map[String, Json]) => {
        val primaryColumns = columns.filter { col => FColumn.findOpt(col) { case retrieve: SlickRetrieve[col.DataType] => retrieve }.map(_.primaryGen.isDefined).getOrElse(false) }
        val jsonData = JsonOperation.readJ(primaryColumns)(v)
        val staticMany = StaticManyOperation.convertList2Query(jsonData)
        for {
          updateInfo <- DeleteOperation.parseInsert(binds, jsonData)
          staticM <- DBIO.from(staticMany)
        } yield {
          updateInfo.copy(many = staticM).effectRows
        }
      },
      updateGen = (v: Map[String, Json]) => {
        val jsonData = JsonOperation.readJ(columns)(v)
        val staticMany = StaticManyOperation.convertList2Query(jsonData)
        for {
          updateInfo <- UpdateOperation.parseInsert(binds, jsonData)
          staticM <- DBIO.from(staticMany)
        } yield {
          updateInfo.copy(many = staticM)
        }
      },
      staticMany = StaticManyOperation.convertList2Ubw(columns)
    )
  }

  def result
  (orderColumn: String, isDesc: Boolean = true)
  (
    implicit
    jsonEv: Query[_, Seq[Any], Seq] => BasicProfile#StreamingQueryActionExtensionMethods[Seq[Seq[Any]], Seq[Any]],
    repToDBIO: Rep[Int] => BasicProfile#QueryActionExtensionMethods[Int, NoStream],
    retrieve: Query[_, String, Seq] => BasicProfile#StreamingQueryActionExtensionMethods[Seq[String], String],
    insertConv: Query[_, Seq[Any], Seq] => JdbcActionComponent#InsertActionExtensionMethods[Seq[Any]],
    deleteConV: Query[RelationalProfile#Table[_], _, Seq] => JdbcActionComponent#DeleteActionExtensionMethods,
    updateConV: Query[_, Seq[Any], Seq] => JdbcActionComponent#UpdateActionExtensionMethods[Seq[Any]],
    ec: ExecutionContext
  ): QueryJsonInfo = {
    result(List(ColumnOrder(orderColumn, isDesc)))
  }

  def result
  (
    implicit
    jsonEv: Query[_, Seq[Any], Seq] => BasicProfile#StreamingQueryActionExtensionMethods[Seq[Seq[Any]], Seq[Any]],
    repToDBIO: Rep[Int] => BasicProfile#QueryActionExtensionMethods[Int, NoStream],
    retrieve: Query[_, String, Seq] => BasicProfile#StreamingQueryActionExtensionMethods[Seq[String], String],
    insertConv: Query[_, Seq[Any], Seq] => JdbcActionComponent#InsertActionExtensionMethods[Seq[Any]],
    deleteConV: Query[RelationalProfile#Table[_], _, Seq] => JdbcActionComponent#DeleteActionExtensionMethods,
    updateConV: Query[_, Seq[Any], Seq] => JdbcActionComponent#UpdateActionExtensionMethods[Seq[Any]],
    ec: ExecutionContext
  ): QueryJsonInfo = {
    result(Nil)
  }*/
}