package net.scalax.fsn.slick.operation

import net.scalax.fsn.core._
import net.scalax.fsn.common.atomic.{ DefaultValue, FProperty }
import net.scalax.fsn.json.operation.{ AtomicValueHelper, FSomeValue }
import net.scalax.fsn.slick.atomic.{ StrNeededFetch, StrOrderNullsLast, StrOrderTargetName, StrSlickSelect }
import net.scalax.fsn.slick.helpers._
import net.scalax.fsn.slick.model._
import shapeless._
import slick.jdbc.JdbcProfile
import slick.lifted.{ ColumnOrdered, FlatShapeLevel, Query, Shape }

import scala.annotation.tailrec
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

object StrOutSelectConvert {

  def ubwGen(wQuery: SlickQueryBindImpl): PileSyntax.PileGen[StrSlickQuery] = {
    Pile.transformTreeList {
      new AtomicQuery(_) {
        val aa = withRep(needAtomic[StrSlickSelect] :: needAtomicOpt[StrNeededFetch] :: needAtomicOpt[StrOrderNullsLast] :: needAtomicOpt[StrOrderTargetName] :: needAtomicOpt[DefaultValue] :: needAtomic[FProperty] :: FANil)
          .mapTo {
            case (slickSelect :: neededFetchOpt :: isOrderNullsLastContent :: orderTargetNameContent :: defaultOpt :: property :: HNil, data) => {
              val isOrderNullsLast = isOrderNullsLastContent.map(_.isOrderNullsLast).getOrElse(true)
              val orderTargetName = orderTargetNameContent.map(_.orderTargetName)
              val isInView = neededFetchOpt.map(_.isInView).getOrElse(true)

              val dataOpt = data match {
                case FSomeValue(s) => Option(s)
                case _ => defaultOpt.map(_.value)
              }
              val filterGen = for {
                eachPri <- slickSelect.filterGen
                eachData <- dataOpt
              } yield {
                new FilterColumnGen[slickSelect.TargetType] {
                  override type BooleanTypeRep = eachPri.BooleanTypeRep
                  override val dataToCondition = (sourceCol: slickSelect.TargetType) => {
                    eachPri.dataToCondition(sourceCol)(eachData)
                  }
                  override val wt = eachPri.wt
                }: FilterColumnGen[slickSelect.TargetType]
              }

              def convertLikeString(str: String) = {
                val addPrefix = if (str.startsWith("%")) {
                  str
                } else {
                  "%" + str
                }
                if (addPrefix.endsWith("%")) {
                  addPrefix
                } else {
                  addPrefix + "%"
                }
              }

              @tailrec
              def unwrapOption2String(s: Any): Option[String] = {
                s match {
                  case Some(t) => unwrapOption2String(t)
                  case t: String => Option(t)
                  case _ => Option.empty
                }
              }

              val likeFilterGen = for {
                eachPri <- slickSelect.likeableGen.toList
                eachData <- unwrapOption2String(dataOpt).toList.flatMap(s => s.split(" ").toList.filter(!_.isEmpty)).map(convertLikeString)
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
                  filterGen.toList ::: likeFilterGen
                )
              else
                StrSReader(
                  slickSelect.outCol,
                  isInView,
                  slickSelect.shape,
                  slickSelect.colToOrder.map(s => property.proName -> ((t: slickSelect.TargetType) => s(t).nullsFirst)),
                  orderTargetName.map(s => property.proName -> s),
                  filterGen.toList ::: likeFilterGen

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
}

trait StrSlickQuery extends AtomicValueHelper {
  val readers: List[StrReaderWithIndex]
  val sortMaps: Map[String, Int]
  val wrapQuery: SlickQueryBindImpl
  val filterGen: List[FilterColumnGen[List[Any]]]

  def slickResult(
    implicit
    slickProfile: JdbcProfile,
    ec: ExecutionContext
  ): SlickParam => ListAnyWrap1111 = {
    slickResult(Nil)
  }

  def slickResult(orderColumn: String, isDesc: Boolean = true)(
    implicit
    slickProfile: JdbcProfile,
    ec: ExecutionContext
  ): SlickParam => ListAnyWrap1111 = {
    slickResult(List(ColumnOrder(orderColumn, isDesc)))
  }

  def slickResult(defaultOrders: List[ColumnOrder])(
    implicit
    slickProfile: JdbcProfile,
    ec: ExecutionContext
  ): SlickParam => ListAnyWrap1111 = {
    (slickParam: SlickParam) =>
      val cols: List[Any] = readers.map(_.reader.sourceCol)
      val shape: Shape[FlatShapeLevel, List[Any], List[Any], List[Any]] = new ListColumnShape[FlatShapeLevel](readers.map(_.reader.mainShape))
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
            val resultArray = Array.fill[AtomicValue](readers.size)(AtomicValueImpl.empty)
            inViewReadersWithIndex.foreach {
              case (reader, index) =>
                resultArray(reader.index) = set(eachRow(index).asInstanceOf[reader.reader.DataType])
            }
            resultArray.toList
          }
          ListAnyCollection1111(resultSet, Option(s._2))
        }
      val profile = slickProfile
      import profile.api._
      ListAnyWrap1111(rs, sortbyQuery2.result.statements.toList)
  }
}