package net.scalax.fsn.slick.operation

import net.scalax.fsn.slick.model.{ SlickPage, SlickParam, SlickRange }
import slick.jdbc.JdbcProfile
import slick.lifted.Query

import scala.concurrent.ExecutionContext

object CommonResult {

  type CommonRType[T] = (List[T], Int)

  def commonResult[E, U](commonQuery: Query[E, U, List], sortedQuery: Query[E, U, List])(
    implicit
    slickProfile: JdbcProfile,
    ec: ExecutionContext
  ): SlickParam => slickProfile.api.DBIO[CommonRType[U]] = {
    val profile = slickProfile
    import profile.api._
    val mappedQuery = commonQuery

    val result: SlickParam => DBIO[CommonRType[U]] = slickParam => {
      try {

        val baseQuery = sortedQuery

        slickParam match {
          case SlickParam(_, Some(SlickRange(drop1, Some(take1))), Some(SlickPage(pageIndex1, pageSize1)), _) =>
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
                (s, endCount - startCount)
              })
            })
              .flatMap(s => s)

          case SlickParam(_, Some(SlickRange(drop, Some(take))), None, _) =>
            val dropQuery = mappedQuery.drop(drop)

            baseQuery.drop(drop).take(take - drop).result.map(s => {
              (s, s.size)
            })

          case SlickParam(_, Some(SlickRange(drop1, None)), Some(SlickPage(pageIndex1, pageSize1)), _) =>
            val startCount = Math.max(0, drop1)
            val pageIndex = Math.max(0, pageIndex1)
            val pageSize = Math.max(0, pageSize1)

            val dropQuery = mappedQuery.drop(startCount)

            (for {
              sum <- dropQuery.size.result
            } yield {

              val limitQuery = baseQuery.drop(startCount).drop(pageIndex * pageSize).take(pageSize)

              limitQuery.result.map(s => {
                (s, sum)
              })
            })
              .flatMap(s => s)

          case SlickParam(_, Some(SlickRange(drop, None)), None, _) =>
            baseQuery.drop(drop).result.map(s => {
              (s, s.size)
            })

          case SlickParam(_, None, Some(SlickPage(pageIndex, pageSize)), _) =>
            val dropQuery = baseQuery.drop(pageIndex * pageSize)
            val takeQuery = dropQuery.take(pageSize)

            for {
              sum <- mappedQuery.size.result
              s <- takeQuery.result
            } yield {
              (s, sum)
            }
          case _ =>
            baseQuery.result.map(s => {
              (s, s.size)
            })
        }

      } catch {
        case e: Exception =>
          DBIO.failed(e)
      }

    }
    result

  }

}