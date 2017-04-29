package net.scalax.fsn.slick.operation

import net.scalax.fsn.core._
import net.scalax.fsn.common.atomic.FProperty
import net.scalax.fsn.slick.atomic.{ OrderNullsLast, OrderTargetName, SlickSelect }
import net.scalax.fsn.slick.helpers.{ ListColumnShape, SlickQueryBindImpl }
import net.scalax.fsn.slick.model.{ ColumnOrder, SlickPage, SlickParam, SlickRange }
import shapeless._
import slick.basic.BasicProfile
import slick.dbio.{ DBIO, NoStream }
import slick.jdbc.JdbcActionComponent
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

  def ubwGen(wQuery: SlickQueryBindImpl): FPileSyntax.PileGen[Option, FSlickQuery] = {
    FPile.transformTreeList {
      new FAtomicQuery(_) {
        val aa = withRep(needAtomic[SlickSelect] :: needAtomicOpt[OrderNullsLast] :: needAtomicOpt[OrderTargetName] :: needAtomic[FProperty] :: HNil)
          .mapToOption {
            case (slickSelect :: isOrderNullsLastContent :: orderTargetNameContent :: property :: HNil, data) => {
              val isOrderNullsLast = isOrderNullsLastContent.map(_.isOrderNullsLast).getOrElse(true)
              val orderTargetName = orderTargetNameContent.map(_.orderTargetName)
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

              slickReaderGen: SlickReader
            }
          }
      }.aa
    } { genList =>
      val gensWithIndex = genList.zipWithIndex
      val genSortMap: Map[String, Seq[Any] => ColumnOrdered[_]] = {
        gensWithIndex
          .toStream
          .map {
            case (gen, index) =>
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
      val finalOrderGen: Map[String, Seq[Any] => ColumnOrdered[_]] = baseOrderTarget.map {
        case (key, value) =>
          key -> genSortMap.get(value).getOrElse(throw new Exception(s"$key 需要映射 $value 的排序方案，但找不到 $value 对应的列的排序"))
      } ++ genSortMap

      val cols: List[Any] = genList.map(_.sourceCol)
      val shape = new ListColumnShape[FlatShapeLevel](genList.map(_.mainShape))
      val selectQuery = wQuery.bind(Query(cols)(shape))

      new FSlickQuery {
        override val uQuery = selectQuery.to[List]
        override val sortMap = finalOrderGen
        override val lineConvert = { list: Seq[Any] =>
          list.toStream.zip(genList).map {
            case (eachData, reader) =>
              Option(reader.convert(eachData.asInstanceOf[reader.SlickType]))
          }.toList
        }
      }
    }
  }

  def ubwGenWithoutData: FPileSyntaxWithoutData.PileGen[Option, List[String]] = {
    FPile.transformTreeListWithoutData {
      new FAtomicQuery(_) {
        val aa = withRep(needAtomic[SlickSelect] :: needAtomicOpt[OrderTargetName] :: needAtomic[FProperty] :: HNil)
          .mapToOptionWithoutData {
            case (slickSelect :: orderTargetNameContent :: property :: HNil) =>
              if (slickSelect.colToOrder.isDefined || orderTargetNameContent.isDefined) {
                Option(property.proName)
              } else {
                None
              }
          }
      }.aa
    } { genList =>
      genList.flatten
    }
  }

}

trait FSlickQuery {

  val uQuery: Query[List[Any], List[Any], List]
  val sortMap: Map[String, Seq[Any] => ColumnOrdered[_]]
  val lineConvert: Seq[Any] => List[Option[Any]]

  def slickResult(
    implicit
    jsonEv: Query[_, List[Any], List] => JdbcActionComponent#StreamingQueryActionExtensionMethods[List[List[Any]], List[Any]],
    repToDBIO: Rep[Int] => JdbcActionComponent#QueryActionExtensionMethods[Int, NoStream],
    ec: ExecutionContext
  ): SlickParam => DBIO[(List[List[Option[Any]]], Int)] = {
    slickResult(Nil)
  }

  def slickResult(orderColumn: String, isDesc: Boolean = true)(
    implicit
    jsonEv: Query[_, List[Any], List] => JdbcActionComponent#StreamingQueryActionExtensionMethods[List[List[Any]], List[Any]],
    repToDBIO: Rep[Int] => JdbcActionComponent#QueryActionExtensionMethods[Int, NoStream],
    ec: ExecutionContext
  ): SlickParam => DBIO[(List[List[Option[Any]]], Int)] = {
    slickResult(List(ColumnOrder(orderColumn, isDesc)))
  }

  def slickResult(defaultOrders: List[ColumnOrder])(
    implicit
    jsonEv: Query[_, List[Any], List] => JdbcActionComponent#StreamingQueryActionExtensionMethods[List[List[Any]], List[Any]],
    repToDBIO: Rep[Int] => JdbcActionComponent#QueryActionExtensionMethods[Int, NoStream],
    ec: ExecutionContext
  ): SlickParam => DBIO[(List[List[Option[Any]]], Int)] = {
    (slickParam: SlickParam) => CommonResult.commonResult(defaultOrders, uQuery, lineConvert, sortMap).apply(slickParam)
  }

}

object CommonResult {

  type CommonRType[T] = (List[T], Int)

  def commonResult[E, U, T](defaultOrders: List[ColumnOrder], query: Query[E, U, List], modelConvert: U => T, sortMap: Map[String, E => ColumnOrdered[_]])(
    implicit
    jsonEv: Query[E, U, List] => JdbcActionComponent#StreamingQueryActionExtensionMethods[List[U], U],
    repToDBIO: Rep[Int] => JdbcActionComponent#QueryActionExtensionMethods[Int, NoStream],
    ec: ExecutionContext
  ): SlickParam => DBIO[CommonRType[T]] = {
    val mappedQuery = query

    val result: SlickParam => DBIO[CommonRType[T]] = slickParam => {
      val autualOrders = defaultOrders ::: slickParam.orders
      val baseQuery = {
        autualOrders.foldLeft(mappedQuery) {
          case (eachQuery, ColumnOrder(eachOrderName, eachIsDesc)) =>
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
              val dataGen = s.map(t => {
                modelConvert(t)
              })
              (dataGen, endCount - startCount)
            })
          })
            .flatMap(s => s)

        case SlickParam(_, Some(SlickRange(drop, Some(take))), None) =>
          val dropQuery = mappedQuery.drop(drop)

          baseQuery.drop(drop).take(take - drop).result.map(s => {
            val dataGen = s.map(t => {
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
              val dataGen = s.map(t => {
                modelConvert(t)
              })
              (dataGen, sum)
            })
          })
            .flatMap(s => s)

        case SlickParam(_, Some(SlickRange(drop, None)), None) =>
          baseQuery.drop(drop).result.map(s => {
            val dataGen = s.map(t => {
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
            val dataGen = s.map(t => {
              modelConvert(t)
            })
            (dataGen, sum)
          }
        case _ =>
          baseQuery.result.map(s => {
            val dataGen = s.map(t => {
              modelConvert(t)
            })
            (dataGen, s.size)
          })
      }
    }

    result

  }

}