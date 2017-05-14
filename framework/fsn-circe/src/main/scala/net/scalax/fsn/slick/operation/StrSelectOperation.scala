package net.scalax.fsn.slick.operation

import net.scalax.fsn.core._
import net.scalax.fsn.common.atomic.FProperty
import net.scalax.fsn.json.operation.FAtomicValueHelper
import net.scalax.fsn.slick.atomic.{ StrNeededFetch, StrOrderNullsLast, StrOrderTargetName, StrSlickSelect }
import net.scalax.fsn.slick.helpers._
import net.scalax.fsn.slick.model._
import shapeless._
import slick.dbio.{ DBIO, NoStream }
import slick.jdbc.JdbcActionComponent
import slick.lifted._

import scala.concurrent.ExecutionContext

trait StrSlickReader {

  type SourceColumn
  type TargetColumn
  type DataType

  val orderGen: Option[(String, TargetColumn => ColumnOrdered[_])]
  val orderTargetGen: Option[(String, String)]
  val sourceCol: SourceColumn

  val inView: Boolean

  val mainShape: Shape[_ <: FlatShapeLevel, SourceColumn, DataType, TargetColumn]
  val primaryGen: List[FilterColumnGen[TargetColumn]]

}

case class StrSReader[S, T, D](
    override val sourceCol: S,
    override val inView: Boolean,
    override val mainShape: Shape[_ <: FlatShapeLevel, S, D, T],
    override val orderGen: Option[(String, T => ColumnOrdered[_])],
    override val orderTargetGen: Option[(String, String)],
    override val primaryGen: List[FilterColumnGen[T]] = Nil
) extends StrSlickReader {

  override type SourceColumn = S
  override type TargetColumn = T
  override type DataType = D

}

case class StrReaderWithIndex(reader: StrSlickReader, index: Int)

object StrOutSelectConvert extends FilterModelHelper {

  def ubwGen(wQuery: SlickQueryBindImpl): FPileSyntax.PileGen[StrSlickQuery] = {
    FPile.transformTreeList {
      new FAtomicQuery(_) {
        val aa = withRep(needAtomic[StrSlickSelect] :: (needAtomicOpt[StrNeededFetch] :: (needAtomicOpt[StrOrderNullsLast] :: needAtomicOpt[StrOrderTargetName] :: FANil) :: FANil) :: needAtomic[FProperty] :: FANil)
          .mapTo {
            case (slickSelect :: (neededFetchOpt :: (isOrderNullsLastContent :: orderTargetNameContent :: HNil) :: HNil) :: property :: HNil, data) => {
              val isOrderNullsLast = isOrderNullsLastContent.map(_.isOrderNullsLast).getOrElse(true)
              val orderTargetName = orderTargetNameContent.map(_.orderTargetName)
              val isInView = neededFetchOpt.map(_.isInView).getOrElse(true)

              val filterGen = for {
                eachPri <- slickSelect.filterGen
                eachData <- data.opt.flatMap(_.eq)
              } yield {
                new FilterColumnGen[slickSelect.TargetType] {
                  override type BooleanTypeRep = eachPri.BooleanTypeRep
                  override val dataToCondition = (sourceCol: slickSelect.TargetType) => {
                    eachPri.dataToCondition(sourceCol)(eachData)
                  }
                  override val wt = eachPri.wt
                }: FilterColumnGen[slickSelect.TargetType]
              }

              val likeFilterGen = for {
                eachPri <- slickSelect.likeableGen
                eachData <- data.opt.flatMap(_.like)
              } yield {
                new FilterColumnGen[slickSelect.TargetType] {
                  override type BooleanTypeRep = eachPri.BooleanTypeRep
                  override val dataToCondition = (sourceCol: slickSelect.TargetType) => {
                    eachPri.dataToCondition(sourceCol, eachData)
                  }
                  override val wt = eachPri.wt
                }: FilterColumnGen[slickSelect.TargetType]
              }

              def slickReaderGen: StrSReader[slickSelect.SourceType, slickSelect.TargetType, slickSelect.DataType] = if (isOrderNullsLast)
                StrSReader(
                  slickSelect.outCol,
                  isInView,
                  slickSelect.shape,
                  slickSelect.colToOrder.map(s => property.proName -> ((t: slickSelect.TargetType) => s(t).nullsLast)),
                  orderTargetName.map(s => property.proName -> s),
                  filterGen.toList ::: likeFilterGen.toList
                )
              else
                StrSReader(
                  slickSelect.outCol,
                  isInView,
                  slickSelect.shape,
                  slickSelect.colToOrder.map(s => property.proName -> ((t: slickSelect.TargetType) => s(t).nullsFirst)),
                  orderTargetName.map(s => property.proName -> s),
                  filterGen.toList ::: likeFilterGen.toList

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

      val slickFilterGen = gensWithIndex.flatMap {
        case StrReaderWithIndex(reader, index) =>
          reader.primaryGen.map(eachPri => new FilterColumnGen[List[Any]] {
            override type BooleanTypeRep = eachPri.BooleanTypeRep
            override val dataToCondition = (cols: List[Any]) => {
              eachPri.dataToCondition(cols(index).asInstanceOf[reader.TargetColumn])
            }
            override val wt = eachPri.wt
          }): List[FilterColumnGen[List[Any]]]
      }

      new StrSlickQuery {
        override val readers = gensWithIndex
        override val sortMaps = finalOrderGen
        override val wrapQuery = wQuery
        override val filterGen = slickFilterGen
      }
    }
  }

  def ubwGenWithoutData: FPileSyntaxWithoutData.PileGen[List[String]] = {
    FPile.transformTreeListWithoutData {
      new FAtomicQuery(_) {
        val aa = withRep(needAtomic[StrSlickSelect] :: needAtomicOpt[StrOrderTargetName] :: needAtomic[FProperty] :: FANil)
          .mapToWithoutData {
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

trait StrSlickQuery extends FAtomicValueHelper {
  val readers: List[StrReaderWithIndex]
  val sortMaps: Map[String, Int]
  val wrapQuery: SlickQueryBindImpl
  val filterGen: List[FilterColumnGen[List[Any]]]

  def slickResult(
    implicit
    jsonEv: Query[_, List[Any], List] => JdbcActionComponent#StreamingQueryActionExtensionMethods[List[List[Any]], List[Any]],
    repToDBIO: Rep[Int] => JdbcActionComponent#QueryActionExtensionMethods[Int, NoStream],
    ec: ExecutionContext
  ): SlickParam => ListAnyWrap1111 = {
    slickResult(Nil)
  }

  def slickResult(orderColumn: String, isDesc: Boolean = true)(
    implicit
    jsonEv: Query[_, List[Any], List] => JdbcActionComponent#StreamingQueryActionExtensionMethods[List[List[Any]], List[Any]],
    repToDBIO: Rep[Int] => JdbcActionComponent#QueryActionExtensionMethods[Int, NoStream],
    ec: ExecutionContext
  ): SlickParam => ListAnyWrap1111 = {
    slickResult(List(ColumnOrder(orderColumn, isDesc)))
  }

  def slickResult(defaultOrders: List[ColumnOrder])(
    implicit
    jsonEv: Query[_, List[Any], List] => JdbcActionComponent#StreamingQueryActionExtensionMethods[List[List[Any]], List[Any]],
    repToDBIO: Rep[Int] => JdbcActionComponent#QueryActionExtensionMethods[Int, NoStream],
    ec: ExecutionContext
  ): SlickParam => ListAnyWrap1111 = {
    (slickParam: SlickParam) =>
      val cols: List[Any] = readers.map(_.reader.sourceCol)
      val shape: Shape[FlatShapeLevel, List[Any], List[Any], List[Any]] = new ListColumnShape[FlatShapeLevel](readers.map(_.reader.mainShape))
      /*try {
        ShapedValue(cols, shape).packedValue(shape.packedShape).toNode
        scala.collection.immutable.Vector
      } catch {
        case e: Exception =>
          e.printStackTrace
          throw e
      }*/
      val selectQuery = wrapQuery.bind(Query(cols)(shape))
      val filterQuery = filterGen.foldLeft(selectQuery) { (query, filterGen) =>
        query.filter(filterGen.dataToCondition)(filterGen.wt)
      }

      val sortedQuery = (slickParam.orders ::: defaultOrders)
        .filter(s => sortMaps.keySet.contains(s.columnName))
        .map { order =>
          sortMaps(order.columnName) -> order.isDesc
        }.foldLeft(filterQuery) {
          case (query, (orderIndex, isDesc)) =>
            query.sortBy { cols =>
              val reader = readers(orderIndex).reader
              val orderInstance = reader.orderGen.get._2.apply(cols(orderIndex).asInstanceOf[reader.TargetColumn])
              if (isDesc) orderInstance.desc else orderInstance.asc
            }(identity)
        }

      val sortbyQuery2 = sortedQuery.to[List]

      val inViewReaders = readers.filter(_.reader.inView == true)
      val inViewReadersWithIndex = inViewReaders.zipWithIndex
      val mapQuery = sortbyQuery2.map(values => inViewReaders.map(s => values(s.index)))(new ListColumnShape[FlatShapeLevel](inViewReaders.map(_.reader.mainShape)))

      val rs = CommonResult.commonResult(filterQuery.to[List], /*sortbyQuery2*/ mapQuery).apply(slickParam)
        .map { s =>
          val resultSet = s._1.map { eachRow =>
            val resultArray = Array.fill[FAtomicValue](readers.size)(FAtomicValueImpl.empty)
            inViewReadersWithIndex.foreach {
              case (reader, index) =>
                resultArray(reader.index) = set(eachRow(index).asInstanceOf[reader.reader.DataType])
            }
            resultArray.toList
          }
          ListAnyCollection1111(resultSet, Option(s._2))
        }
      ListAnyWrap1111(rs, sortbyQuery2.result.statements.toList)
  }
}

object CommonResult {

  type CommonRType[T] = (List[T], Int)

  def commonResult[E, U](commonQuery: Query[E, U, List], sortedQuery: Query[E, U, List])(
    implicit
    jsonEv: Query[E, U, List] => JdbcActionComponent#StreamingQueryActionExtensionMethods[List[U], U],
    repToDBIO: Rep[Int] => JdbcActionComponent#QueryActionExtensionMethods[Int, NoStream],
    ec: ExecutionContext
  ): SlickParam => DBIO[CommonRType[U]] = {
    val mappedQuery = commonQuery

    val result: SlickParam => DBIO[CommonRType[U]] = slickParam => {
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
    }

    result

  }

}