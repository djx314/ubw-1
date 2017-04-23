package net.scalax.fsn.slick.operation

import net.scalax.fsn.core._
import net.scalax.fsn.common.atomic.FProperty
import net.scalax.fsn.slick.atomic.{ StrOrderNullsLast, StrOrderTargetName, StrSlickSelect }
import net.scalax.fsn.slick.helpers.{ ListColumnShape, SlickQueryBindImpl }
import net.scalax.fsn.slick.model._
import shapeless._
import slick.basic.BasicProfile
import slick.dbio.{ DBIO, NoStream }
import slick.jdbc.JdbcActionComponent
import slick.lifted._

import scala.language.existentials
import scala.concurrent.ExecutionContext

trait StrSlickReader {

  type SourceColumn
  //type SlickType
  type TargetColumn
  type DataType

  val orderGen: Option[(String, TargetColumn => ColumnOrdered[_])]
  val orderTargetGen: Option[(String, String)]
  val sourceCol: SourceColumn
  //val convert: SlickType => DataType

  val mainShape: Shape[_ <: FlatShapeLevel, SourceColumn, DataType, TargetColumn]

}

case class StrSReader[S, T, D](
    override val sourceCol: S,
    override val mainShape: Shape[_ <: FlatShapeLevel, S, D, T],
    override val orderGen: Option[(String, T => ColumnOrdered[_])],
    override val orderTargetGen: Option[(String, String)]
) extends StrSlickReader {

  override type SourceColumn = S
  override type TargetColumn = T
  override type DataType = D

}

case class StrReaderWithIndex(reader: StrSlickReader, index: Int)

object StrOutSelectConvert {

  def ubwGen(wQuery: SlickQueryBindImpl): FPileSyntax.PileGen[Option, StrSlickQuery] = {
    FPile.transformTreeList {
      new FAtomicQuery(_) {
        val aa = withRep(needAtomic[StrSlickSelect] :: needAtomicOpt[StrOrderNullsLast] :: needAtomicOpt[StrOrderTargetName] :: needAtomic[FProperty] :: HNil)
          .mapToOption {
            case (slickSelect :: isOrderNullsLastContent :: orderTargetNameContent :: property :: HNil, data) => {
              val isOrderNullsLast = isOrderNullsLastContent.map(_.isOrderNullsLast).getOrElse(true)
              val orderTargetName = orderTargetNameContent.map(_.orderTargetName)
              def slickReaderGen: StrSReader[slickSelect.SourceType, slickSelect.TargetType, slickSelect.DataType] = if (isOrderNullsLast)
                StrSReader(
                  slickSelect.outCol,
                  slickSelect.shape,
                  slickSelect.colToOrder.map(s => property.proName -> ((t: slickSelect.TargetType) => s(t).nullsLast)),
                  orderTargetName.map(s => property.proName -> s)
                )
              else
                StrSReader(
                  slickSelect.outCol,
                  slickSelect.shape,
                  slickSelect.colToOrder.map(s => property.proName -> ((t: slickSelect.TargetType) => s(t).nullsFirst)),
                  orderTargetName.map(s => property.proName -> s)
                )

              slickReaderGen: StrSlickReader
            }
          }
      }.aa
    } { genList =>
      val gensWithIndex = genList.zipWithIndex.map { case (reader, index) => StrReaderWithIndex(reader, index) }
      val genSortMap: Map[String, Int] = {
        gensWithIndex
          .toStream
          .map {
            case StrReaderWithIndex(reader, index) =>
              reader.orderGen.map {
                case (key, _) =>
                  key -> index
              }
          }
          .collect { case Some(s) => s }
          .toMap
      }

      val baseOrderTarget = genList.toStream.filter(_.orderTargetGen.isDefined).map(_.orderTargetGen.get).toMap
      val finalOrderGen: Map[String, Int] = baseOrderTarget.map {
        case (key, value) =>
          key -> genSortMap.get(value).getOrElse(throw new Exception(s"$key 需要映射 $value 的排序方案，但找不到 $value 对应的列的排序"))
      } ++ genSortMap

      new StrSlickQuery {
        override val readers = gensWithIndex
        override val sortMaps = finalOrderGen
        override val wrapQuery = wQuery
      }

      /*val genSortMap: Map[String, Seq[Any] => ColumnOrdered[_]] = {
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

      val cols: Seq[Any] = genList.map(_.sourceCol)
      val shape = new ListAnyShape[FlatShapeLevel](genList.map(_.mainShape))
      val selectQuery = wQuery.bind(Query(cols)(shape))

      new StrFSlickQuery {
        override val uQuery = selectQuery
        override val sortMap = finalOrderGen
        override val lineConvert = { list: Seq[Any] =>
          list.map(Option(_)).toList
        }
      }*/
    }
  }

  def ubwGenWithoutData /*(wQuery: SlickQueryBindImpl)*/ : FPileSyntaxWithoutData.PileGen[Option, List[String]] = {
    FPile.transformTreeListWithoutData {
      new FAtomicQuery(_) {
        val aa = withRep(needAtomic[StrSlickSelect] :: needAtomicOpt[StrOrderTargetName] :: needAtomic[FProperty] :: HNil)
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

trait StrSlickQuery {
  val readers: List[StrReaderWithIndex]
  val sortMaps: Map[String, Int]
  val wrapQuery: SlickQueryBindImpl

  def slickResult(
    implicit
    jsonEv: Query[_, List[Any], List] => JdbcActionComponent#StreamingQueryActionExtensionMethods[List[List[Any]], List[Any]],
    repToDBIO: Rep[Int] => JdbcActionComponent#QueryActionExtensionMethods[Int, NoStream],
    ec: ExecutionContext
  ): SlickParam => ListAnyWrap = {
    slickResult(Nil)
  }

  def slickResult(orderColumn: String, isDesc: Boolean = true)(
    implicit
    jsonEv: Query[_, List[Any], List] => JdbcActionComponent#StreamingQueryActionExtensionMethods[List[List[Any]], List[Any]],
    repToDBIO: Rep[Int] => JdbcActionComponent#QueryActionExtensionMethods[Int, NoStream],
    ec: ExecutionContext
  ): SlickParam => ListAnyWrap = {
    slickResult(List(ColumnOrder(orderColumn, isDesc)))
  }

  def slickResult(defaultOrders: List[ColumnOrder])(
    implicit
    jsonEv: Query[_, List[Any], List] => JdbcActionComponent#StreamingQueryActionExtensionMethods[List[List[Any]], List[Any]],
    repToDBIO: Rep[Int] => JdbcActionComponent#QueryActionExtensionMethods[Int, NoStream],
    ec: ExecutionContext
  ): SlickParam => ListAnyWrap = {
    (slickParam: SlickParam) =>
      val cols: List[Any] = readers.map(_.reader.sourceCol)
      val shape: Shape[FlatShapeLevel, List[Any], List[Any], List[Any]] = new ListColumnShape[FlatShapeLevel](readers.map(_.reader.mainShape))
      try {
        ShapedValue(cols, shape).packedValue(shape.packedShape).toNode
        scala.collection.immutable.Vector
      } catch {
        case e: Exception =>
          e.printStackTrace
          throw e
      }
      val selectQuery = wrapQuery.bind(Query(cols)(shape))
      val sortedQuery = (slickParam.orders ::: defaultOrders)
        .filter(s => sortMaps.keySet.contains(s.columnName))
        .map { order =>
          sortMaps(order.columnName) -> order.isDesc
        }.foldLeft(selectQuery) {
          case (query, (orderIndex, isDesc)) =>
            query.sortBy { cols =>
              val reader = readers(orderIndex).reader
              val orderInstance = reader.orderGen.get._2.apply(cols(orderIndex).asInstanceOf[reader.TargetColumn])
              if (isDesc) orderInstance.desc else orderInstance.asc
            }(identity)
        }

      val sortbyQuery2 = sortedQuery.to[List]
      val rs = CommonResult1111.commonResult(selectQuery.to[List], sortbyQuery2).apply(slickParam)
        .map { s =>
          ListAnyCollection(s._1.map(t => t.map(u => Option(u))), Option(s._2))
        }
      ListAnyWrap(rs, sortbyQuery2.result.statements.toList)
  }
}
/*trait StrFSlickQuery {

  val uQuery: Query[Seq[Any], Seq[Any], Seq]
  val sortMap: Map[String, Seq[Any] => ColumnOrdered[_]]
  val lineConvert: Seq[Any] => List[Option[Any]]

  def slickResult(
    implicit
    jsonEv: Query[_, Seq[Any], List] => JdbcActionComponent#StreamingQueryActionExtensionMethods[List[Seq[Any]], Seq[Any]],
    repToDBIO: Rep[Int] => JdbcActionComponent#QueryActionExtensionMethods[Int, NoStream],
    ec: ExecutionContext
  ): SlickParam => DBIO[(List[List[Option[Any]]], Int)] = {
    slickResult(Nil)
  }

  def slickResult(orderColumn: String, isDesc: Boolean = true)(
    implicit
    jsonEv: Query[_, Seq[Any], List] => JdbcActionComponent#StreamingQueryActionExtensionMethods[List[Seq[Any]], Seq[Any]],
    repToDBIO: Rep[Int] => JdbcActionComponent#QueryActionExtensionMethods[Int, NoStream],
    ec: ExecutionContext
  ): SlickParam => DBIO[(List[List[Option[Any]]], Int)] = {
    slickResult(List(ColumnOrder(orderColumn, isDesc)))
  }

  def slickResult(defaultOrders: List[ColumnOrder])(
    implicit
    jsonEv: Query[_, Seq[Any], List] => JdbcActionComponent#StreamingQueryActionExtensionMethods[List[Seq[Any]], Seq[Any]],
    repToDBIO: Rep[Int] => JdbcActionComponent#QueryActionExtensionMethods[Int, NoStream],
    ec: ExecutionContext
  ): SlickParam => DBIO[(List[List[Option[Any]]], Int)] = {
    (slickParam: SlickParam) =>
      CommonResult1111.commonResult(uQuery.to[List], uQuery.to[List]).apply(slickParam)
        .map(s => s.copy(_1 = s._1.map(t => t.toList.map(u => Option(u)))))
  }

}*/
object CommonResult1111 {

  type CommonRType[T] = (List[T], Int)

  def commonResult[E, U](commonQuery: Query[E, U, List], sortedQuery: Query[E, U, List])(
    implicit
    jsonEv: Query[E, U, List] => JdbcActionComponent#StreamingQueryActionExtensionMethods[List[U], U],
    repToDBIO: Rep[Int] => JdbcActionComponent#QueryActionExtensionMethods[Int, NoStream],
    ec: ExecutionContext
  ): SlickParam => DBIO[CommonRType[U]] = {
    val mappedQuery = commonQuery

    val result: SlickParam => DBIO[CommonRType[U]] = slickParam => {
      //val autualOrders = defaultOrders ::: slickParam.orders
      val baseQuery = sortedQuery
      /*{
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
      }*/

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
              /*val dataGen = s.toList.map(t => {
                modelConvert(t)
              })
              (dataGen, endCount - startCount)*/
              (s, endCount - startCount)
            })
          })
            .flatMap(s => s)

        case SlickParam(_, Some(SlickRange(drop, Some(take))), None) =>
          val dropQuery = mappedQuery.drop(drop)

          baseQuery.drop(drop).take(take - drop).result.map(s => {
            /*val dataGen = s.toList.map(t => {
              modelConvert(t)
            })
            (dataGen, s.size)*/
            (s, s.size)
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
              /*val dataGen = s.toList.map(t => {
                modelConvert(t)
              })
              (dataGen, sum)*/
              (s, sum)
            })
          })
            .flatMap(s => s)

        case SlickParam(_, Some(SlickRange(drop, None)), None) =>
          baseQuery.drop(drop).result.map(s => {
            /*val dataGen = s.toList.map(t => {
              modelConvert(t)
            })
            (dataGen, s.size)*/
            (s, s.size)
          })

        case SlickParam(_, None, Some(SlickPage(pageIndex, pageSize))) =>
          val dropQuery = baseQuery.drop(pageIndex * pageSize)
          val takeQuery = dropQuery.take(pageSize)

          for {
            sum <- mappedQuery.size.result
            s <- takeQuery.result
          } yield {
            /*val dataGen = s.toList.map(t => {
              modelConvert(t)
            })
            (dataGen, sum)*/
            (s, sum)
          }
        case _ =>
          baseQuery.result.map(s => {
            /*val dataGen = s.toList.map(t => {
              modelConvert(t)
            })
            (dataGen, s.size)*/
            (s, s.size)
          })
      }
    }

    result

  }

}