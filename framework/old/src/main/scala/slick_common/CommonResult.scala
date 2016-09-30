package net.scalax.fsn.slick_common

import net.scalax.fsn.model.{ColumnOrder, SlickPage, SlickParam, SlickRange}
import slick.basic.BasicProfile
import slick.dbio._
import slick.lifted._

import scala.concurrent.ExecutionContext

trait CommonResult {

  type CommonRType[T] = (/*List[PropertyInfo],*/List[T], Int)

  def commonResult[E, U, T](defaultOrders: List[ColumnOrder], query: Query[E, U, Seq], modelConvert: U => T, sortMap: Map[String, E => ColumnOrdered[_]]/*, properties: List[PropertyInfo]*/)(
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
              (/*properties,*/dataGen, endCount - startCount)
            })
          })
          .flatMap(s => s)

        case SlickParam(_, Some(SlickRange(drop, Some(take))), None) =>
          val dropQuery = mappedQuery.drop(drop)
          //val takeQuery = dropQuery.take(take)

          baseQuery.drop(drop).take(take - drop).result.map(s => {
            val dataGen = s.toList.map(t => {
              modelConvert(t)
            })
            (/*properties,*/dataGen, s.size)
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
              (/*properties,*/dataGen, sum)
            })
          })
          .flatMap(s => s)

        case SlickParam(_, Some(SlickRange(drop, None)), None) =>
          baseQuery.drop(drop).result.map(s => {
            val dataGen = s.toList.map(t => {
              modelConvert(t)
            })
            (/*properties,*/dataGen, s.size)
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
            (/*properties,*/dataGen, sum)
          }
        case _ =>
          baseQuery.result.map(s => {
            val dataGen = s.toList.map(t => {
              modelConvert(t)
            })
            (/*properties,*/dataGen, s.size)
          })
      }
    }

    result

  }

}

object CommonResult extends CommonResult