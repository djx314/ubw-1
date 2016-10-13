package net.scalax.fsn.slick.operation

import net.scalax.fsn.core._
import net.scalax.fsn.common.atomic.FProperty
import net.scalax.fsn.slick.atomic.{OrderNullsLast, OrderTargetName, SlickSelect, SubUbw}
import net.scalax.fsn.slick.helpers.{ListAnyShape, SlickQueryBindImpl}
import net.scalax.fsn.slick.model.{ColumnOrder, SlickPage, SlickParam, SlickRange}
import slick.basic.BasicProfile
import slick.dbio.{DBIO, NoStream}
import slick.lifted._

import scala.language.existentials
import scala.concurrent.ExecutionContext

trait SlickReader {

  type SourceColumn
  type SlickType
  type TargetColumn
  type DataType

  val orderGen: Option[(String, TargetColumn => ColumnOrdered[_])]
  val orderTargetGen: Option[(String, String)]
  val sourceCol: SourceColumn
  val convert: SlickType => DataType

  val mainShape: Shape[_ <: FlatShapeLevel, SourceColumn, SlickType, TargetColumn]

}

case class SReader[S, U, T, D](
  override val sourceCol: S,
  override val mainShape: Shape[_ <: FlatShapeLevel, S, U, T],
  override val orderGen: Option[(String, T => ColumnOrdered[_])],
  override val orderTargetGen: Option[(String, String)],
  override val convert: U => D
) extends SlickReader {

  override type SourceColumn = S
  override type SlickType = U
  override type TargetColumn = T
  override type DataType = D

}

object OutSelectConvert {

  def convert(column: FColumn): SlickReader = {
    val slickSelect = FColumn.find(column) { case s: SlickSelect[column.DataType] => s }
    val isOrderNullsLast = FColumn.findOpt(column) { case s: OrderNullsLast[column.DataType] => s }.map(_.isOrderNullsLast).getOrElse(true)
    val orderTargetName = FColumn.findOpt(column) { case s: OrderTargetName[column.DataType] => s }.map(_.orderTargetName)
    val property = FColumn.find(column) { case s: FProperty[column.DataType] => s }

    def slickReaderGen: SReader[slickSelect.SourceType, slickSelect.SlickType, slickSelect.TargetType, slickSelect.DataType] = if (isOrderNullsLast)
      SReader(
        slickSelect.outCol,
        slickSelect.shape,
        slickSelect.colToOrder.map(s => property.proName -> ((t: slickSelect.TargetType) => s(t).nullsLast)),
        orderTargetName.map(s => property.proName -> s),
        slickSelect.outConvert
      )
    else
      SReader(
        slickSelect.outCol,
        slickSelect.shape,
        slickSelect.colToOrder.map(s => property.proName -> ((t: slickSelect.TargetType) => s(t).nullsFirst)),
        orderTargetName.map(s => property.proName -> s),
        slickSelect.outConvert
      )

    slickReaderGen
  }

  def extraSubCol(column: List[FColumn]): List[FColumn] = {
    val extraColumns = column.map { s =>
      val subCols = FColumn.findOpt(s) { case t: SubUbw[s.DataType] => t }
      subCols match {
        case Some(t) => t.subCols
        case _ => List(s)
      }
    }.flatten
    if (extraColumns.exists(s => FColumn.findOpt(s) { case t: SubUbw[s.DataType] => t }.isDefined)) {
      extraSubCol(extraColumns)
    } else {
      extraColumns
    }
  }

}

object SelectOperation {

  def encode(columns: List[FColumn], wQuery: SlickQueryBindImpl): JsonQuery = {
    val genList = columns.map(OutSelectConvert.convert)
    val gensWithIndex = genList.zipWithIndex
    val genSortMap: Map[String, Seq[Any] => ColumnOrdered[_]] = {
      gensWithIndex
        .toStream
        .map { case (gen, index) =>
          gen.orderGen.map { order =>
            order._1 -> { cols: Seq[Any] =>
              order._2(cols(index).asInstanceOf[gen.TargetColumn])
            }
          }
        }
        .collect { case Some(s) => s }
        .toMap
    }
    val baseOrderTarget = genList.toStream.filter(_.orderTargetGen.isDefined).map(_.orderTargetGen.get).toMap
    val finalOrderGen: Map[String, Seq[Any] => ColumnOrdered[_]] = baseOrderTarget.map { case (key, value) =>
      key -> genSortMap.get(value).getOrElse(throw new Exception(s"$key 需要映射 $value 的排序方案，但找不到 $value 对应的列的排序"))
    } ++ genSortMap

    val cols: Seq[Any] = genList.map(_.sourceCol)
    val shape = new ListAnyShape[FlatShapeLevel](genList.map(_.mainShape))
    val selectQuery = wQuery.bind(Query(cols)(shape))

    val resultGen: Seq[Any] => List[FColumn] = { s =>
      s.zip(columns).zip(genList).map { case ((result, column), gen) =>
        FsnColumn(column.cols, Option(gen.convert(result.asInstanceOf[gen.SlickType]).asInstanceOf[column.DataType]))
      }.toList
    }

    new JsonQuery {
      override val uQuery = selectQuery
      override val render = resultGen
      override val sortMap = finalOrderGen
    }
  }

}

trait JsonQuery {

  val uQuery: Query[Seq[Any], Seq[Any], Seq]
  val render: Seq[Any] => List[FColumn]
  val sortMap: Map[String, Seq[Any] => ColumnOrdered[_]]

  def jsonResult(
                  implicit
                  jsonEv: Query[_, Seq[Any], Seq] => BasicProfile#StreamingQueryActionExtensionMethods[Seq[Seq[Any]], Seq[Any]],
                  repToDBIO: Rep[Int] => BasicProfile#QueryActionExtensionMethods[Int, NoStream],
                  ec: ExecutionContext
                ): SlickParam => DBIO[(List[List[FColumn]], Int)] = {
    jsonResult(Nil)
  }

  def jsonResult(orderColumn: String, isDesc: Boolean = true)(
    implicit
    jsonEv: Query[_, Seq[Any], Seq] => BasicProfile#StreamingQueryActionExtensionMethods[Seq[Seq[Any]], Seq[Any]],
    repToDBIO: Rep[Int] => BasicProfile#QueryActionExtensionMethods[Int, NoStream],
    ec: ExecutionContext
  ): SlickParam => DBIO[(List[List[FColumn]], Int)] = {
    jsonResult(List(ColumnOrder(orderColumn, isDesc)))
  }

  def jsonResult(defaultOrders: List[ColumnOrder])(
    implicit
    jsonEv: Query[_, Seq[Any], Seq] => BasicProfile#StreamingQueryActionExtensionMethods[Seq[Seq[Any]], Seq[Any]],
    repToDBIO: Rep[Int] => BasicProfile#QueryActionExtensionMethods[Int, NoStream],
    ec: ExecutionContext
  ): SlickParam => DBIO[(List[List[FColumn]], Int)] = {
    (slickParam: SlickParam) => CommonResult.commonResult(defaultOrders, uQuery, render, sortMap).apply(slickParam)
  }

}

object CommonResult {

  type CommonRType[T] = (List[T], Int)

  def commonResult[E, U, T](defaultOrders: List[ColumnOrder], query: Query[E, U, Seq], modelConvert: U => T, sortMap: Map[String, E => ColumnOrdered[_]])(
    implicit
    jsonEv: Query[E, U, Seq] => BasicProfile#StreamingQueryActionExtensionMethods[Seq[U], U],
    repToDBIO: Rep[Int] => BasicProfile#QueryActionExtensionMethods[Int, NoStream],
    ec: ExecutionContext
  ): SlickParam => DBIO[CommonRType[T]] = {
    val mappedQuery = query

    val result: SlickParam => DBIO[CommonRType[T]] = slickParam => {
      val autualOrders = defaultOrders ::: slickParam.orders
      val baseQuery = {
        autualOrders.foldLeft(mappedQuery) { case (eachQuery, ColumnOrder(eachOrderName, eachIsDesc)) =>
          sortMap.get(eachOrderName) match {
            case Some(convert) =>
              eachQuery.sortBy { s =>
                val colOrder = convert(s)

                if (eachIsDesc)
                  colOrder.desc
                else
                  colOrder.asc
              }
            case _ =>
              eachQuery
          }
        }
      }

      slickParam match {
        case SlickParam(_, Some(SlickRange(drop1, Some(take1))), Some(SlickPage(pageIndex1, pageSize1))) =>
          val startCount = Math.max(0, drop1)
          val pageIndex = Math.max(0, pageIndex1)
          val pageSize = Math.max(0, pageSize1)

          val dropQuery = mappedQuery.drop(startCount)

          (for {
            sum <- dropQuery.size.result
          } yield {
            val pageStart = startCount + pageIndex * pageSize
            val pageEnd = pageStart + pageSize
            val endCount = Math.min(take1, startCount + sum)
            val autalStart = Math.max(pageStart, startCount)
            val autalEnd = Math.min(pageEnd, endCount)
            val autalLimit = Math.max(0, autalEnd - autalStart)

            val limitQuery = baseQuery.drop(startCount).drop(pageIndex * pageSize).take(autalLimit)

            limitQuery.result.map(s => {
              val dataGen = s.toList.map(t => {
                modelConvert(t)
              })
              (dataGen, endCount - startCount)
            })
          })
            .flatMap(s => s)

        case SlickParam(_, Some(SlickRange(drop, Some(take))), None) =>
          val dropQuery = mappedQuery.drop(drop)

          baseQuery.drop(drop).take(take - drop).result.map(s => {
            val dataGen = s.toList.map(t => {
              modelConvert(t)
            })
            (dataGen, s.size)
          })

        case SlickParam(_, Some(SlickRange(drop1, None)), Some(SlickPage(pageIndex1, pageSize1))) =>
          val startCount = Math.max(0, drop1)
          val pageIndex = Math.max(0, pageIndex1)
          val pageSize = Math.max(0, pageSize1)

          val dropQuery = mappedQuery.drop(startCount)

          (for {
            sum <- dropQuery.size.result
          } yield {

            val limitQuery = baseQuery.drop(startCount).drop(pageIndex * pageSize).take(pageSize)

            limitQuery.result.map(s => {
              val dataGen = s.toList.map(t => {
                modelConvert(t)
              })
              (dataGen, sum)
            })
          })
            .flatMap(s => s)

        case SlickParam(_, Some(SlickRange(drop, None)), None) =>
          baseQuery.drop(drop).result.map(s => {
            val dataGen = s.toList.map(t => {
              modelConvert(t)
            })
            (dataGen, s.size)
          })

        case SlickParam(_, None, Some(SlickPage(pageIndex, pageSize))) =>
          val dropQuery = baseQuery.drop(pageIndex * pageSize)
          val takeQuery = dropQuery.take(pageSize)

          for {
            sum <- mappedQuery.size.result
            s <- takeQuery.result
          } yield {
            val dataGen = s.toList.map(t => {
              modelConvert(t)
            })
            (dataGen, sum)
          }
        case _ =>
          baseQuery.result.map(s => {
            val dataGen = s.toList.map(t => {
              modelConvert(t)
            })
            (dataGen, s.size)
          })
      }
    }

    result

  }

}