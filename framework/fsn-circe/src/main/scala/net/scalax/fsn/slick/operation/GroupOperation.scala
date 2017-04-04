package net.scalax.fsn.slick.operation

import net.scalax.fsn.core._
import net.scalax.fsn.common.atomic.FProperty
import net.scalax.fsn.slick.atomic._
import net.scalax.fsn.slick.helpers.{ ListColumnShape, SlickQueryBindImpl }
import net.scalax.fsn.slick.model._
import shapeless._
import slick.ast.{ BaseTypedType, TypedType }
import slick.basic.BasicProfile
import slick.dbio.{ DBIO, NoStream }
import slick.jdbc.JdbcActionComponent
import slick.lifted._

import scala.language.existentials
import scala.concurrent.ExecutionContext

sealed abstract trait GroupWraperBase[T] {
  type TargetType = T
  type RepDataType

  val groupModel: Shape[_ <: FlatShapeLevel, Rep[Option[RepDataType]], Option[RepDataType], Rep[Option[RepDataType]]]
  val colToOrder: Option[Rep[Option[RepDataType]] => ColumnOrdered[_]]
  val baseTypedType: BaseTypedType[RepDataType]
  val typedType: TypedType[Option[RepDataType]]

}

trait GroupWraperWithOption[T] extends GroupWraperBase[T] {
  val targetColConvert: TargetType => Rep[Option[RepDataType]]
}

trait GroupWraperWithNonOption[T] extends GroupWraperBase[T] {
  val targetColConvert: TargetType => Rep[RepDataType]
  val groupNoneOptionShape: Shape[_ <: FlatShapeLevel, Rep[RepDataType], RepDataType, Rep[RepDataType]] = {
    //implicitly[Shape[FlatShapeLevel, Rep[RepDataType], RepDataType, Rep[RepDataType]]]
    Shape.repColumnShape[RepDataType, FlatShapeLevel](baseTypedType)
  }
}

trait GroupSlickReader {

  type BaseDataType

  val propertyName: String
  val selectModel: GroupSlickSelect[BaseDataType]
  val groupModel: Option[GroupWraperBase[selectModel.TargetType]]

}

object GroupSelectConvert extends FAtomicGenHelper with FAtomicShapeHelper {

  def ubwGen(wQuery1: SlickQueryBindImpl): FPileSyntax.PileGen[Option, FGroupQuery] = {
    FPile.transformTreeList { path =>
      FAtomicQuery(needAtomicOpt[GroupSlickSelect] :: needAtomicOpt[GroupableColumnBase] :: needAtomic[FProperty] :: HNil)
        .mapToOption(path) {
          case (selectOpt :: groupColOpt :: property :: HNil, data) => {
            val aa = (selectOpt -> groupColOpt) match {
              case (Some(t), None) =>
                new GroupSlickReader {

                  override val propertyName = property.proName
                  override type BaseDataType = path.DataType
                  override val selectModel = t

                  override val groupModel = Option.empty[GroupWraperBase[selectModel.TargetType]]

                }
              case (_, Some(t)) =>
                new GroupSlickReader {

                  override val propertyName = property.proName
                  override type BaseDataType = t.selectModel.DataType
                  override val selectModel = t.selectModel

                  override val groupModel = Option {
                    t match {
                      case a: GroupableNoOptionColumn[path.DataType] =>
                        new GroupWraperWithNonOption[selectModel.TargetType] {
                          override type RepDataType = a.RepType

                          override val groupModel = a.groupModel
                          override val colToOrder = a.colToOrder
                          override val baseTypedType = a.baseTypedType
                          override val typedType = a.typedType
                          override val targetColConvert = a.targetColConvert.asInstanceOf[t.TargetType => Rep[RepDataType]]
                        }
                      case a: GroupableOptionColumn[path.DataType] =>
                        new GroupWraperWithOption[selectModel.TargetType] {
                          override type RepDataType = a.RepType

                          override val groupModel = a.groupModel
                          override val colToOrder = a.colToOrder
                          override val baseTypedType = a.baseTypedType
                          override val typedType = a.typedType
                          override val targetColConvert = a.targetColConvert.asInstanceOf[t.TargetType => Rep[Option[RepDataType]]]
                        }
                    }
                  }

                }
              case _ => throw new Exception("不可预测的原子集合")
            }
            aa: GroupSlickReader
          }
        }
    } { genList =>
      new FGroupQuery {
        override val wQuery = wQuery1
        override val readers = genList.zipWithIndex
      }
    }
  }

}

case class GroupResult(action: DBIO[List[List[Option[Any]]]], statements: List[String])

trait FGroupQuery {
  val wQuery: SlickQueryBindImpl
  val readers: List[(GroupSlickReader, Int)]

  def result(param: GroupParam)(
    implicit
    jsonEv: Query[_, List[Any], List] => JdbcActionComponent#StreamingQueryActionExtensionMethods[List[List[Any]], List[Any]],
    //repToDBIO: Rep[Int] => JdbcActionComponent#QueryActionExtensionMethods[Int, NoStream],
    ec: ExecutionContext
  ): GroupResult = {
    val baseQuery = wQuery.bind(Query(readers.map(_._1.selectModel.outCol))(new ListColumnShape[FlatShapeLevel](readers.map(_._1.selectModel.shape))))
    val keySize = param.keys.size
    val keyIndexs = param.keys.map { s =>
      readers.find(_._1.propertyName == s).get._2
    }
    val groupQuery = baseQuery.groupBy { s =>
      keyIndexs.map(index => s(index))
    }(
      { new ListColumnShape[FlatShapeLevel](keyIndexs.map(index => readers(index)._1.selectModel.shape.packedShape)) },
      { new ListColumnShape[FlatShapeLevel](readers.map(_._1.selectModel.shape.packedShape)) }
    )

    val aggregateIndexsAndMethods = param.aggregates.map { s =>
      s.method -> readers.find(_._1.propertyName == s.property).get
    }

    val resultQuery = groupQuery.map {
      case (keys, queries) =>
        keys :::
          aggregateIndexsAndMethods.map {
            case (method, reader) =>
              val helper = new ExtensionMethodConversions {}
              val groupModel = reader._1.groupModel
              groupModel match {
                case Some(a: GroupWraperWithOption[reader._1.selectModel.TargetType]) =>
                  val eachValue = queries.map { values =>
                    a.targetColConvert(values(reader._2).asInstanceOf[a.TargetType])
                  }(a.groupModel)
                  method match {
                    case "min" =>
                      helper.singleOptionColumnQueryExtensionMethods(eachValue)(a.baseTypedType).min(a.typedType)
                    case "max" =>
                      helper.singleOptionColumnQueryExtensionMethods(eachValue)(a.baseTypedType).max(a.typedType)
                    case "avg" =>
                      helper.singleOptionColumnQueryExtensionMethods(eachValue)(a.baseTypedType).avg(a.typedType)
                    case "sum" =>
                      helper.singleOptionColumnQueryExtensionMethods(eachValue)(a.baseTypedType).sum(a.typedType)
                  }
                case Some(a: GroupWraperWithNonOption[reader._1.selectModel.TargetType]) =>
                  val eachValue = queries.map { values =>
                    a.targetColConvert(values(reader._2).asInstanceOf[a.TargetType])
                  }(a.groupNoneOptionShape)
                  method match {
                    case "min" =>
                      helper.singleColumnQueryExtensionMethods(eachValue)(a.baseTypedType).min(a.typedType)
                    case "max" =>
                      helper.singleColumnQueryExtensionMethods(eachValue)(a.baseTypedType).max(a.typedType)
                    case "avg" =>
                      helper.singleColumnQueryExtensionMethods(eachValue)(a.baseTypedType).avg(a.typedType)
                    case "sum" =>
                      helper.singleColumnQueryExtensionMethods(eachValue)(a.baseTypedType).sum(a.typedType)
                  }
                /*trait GroupWraperWithOption[T] extends GroupWraperBase[T] {
                  val targetColConvert: T => Rep[Option[RepDataType]]
                  }
                  trait GroupWraperWithNonOption[T] extends GroupWraperBase[T] {
                  val targetColConvert: T => Rep[RepDataType]
                  }*/
              }
          }
    } {
      val aa = keyIndexs.map(index => readers(index)._1.selectModel.shape.packedShape.asInstanceOf[Shape[FlatShapeLevel, Any, Any, Any]])
      val bb = aggregateIndexsAndMethods.map {
        case (method, reader) =>
          reader._1.groupModel.get.groupModel.asInstanceOf[Shape[FlatShapeLevel, Any, Any, Any]]
      }
      new ListColumnShape[FlatShapeLevel](aa ::: bb)
    }

    val action = jsonEv(resultQuery.to[List]).result.map { s =>
      //val sumSize = keySize + aggregateIndexsAndMethods.size
      val result = s.map { t =>
        val initArray = Array.fill[Option[Any]](readers.size)(Option.empty[Any])
        keyIndexs.zipWithIndex.map {
          case (keyIndexs, resultIndex) =>
            initArray(keyIndexs) = Option(t(resultIndex))
        }
        aggregateIndexsAndMethods.zipWithIndex.map {
          case (aggregate, resultIndex) =>
            initArray(aggregate._2._2) = Option(t(resultIndex + keySize))
        }
        initArray.toList
      }
      result
    }
    GroupResult(action, resultQuery.to[List].result.statements.toList)
  }
}
/*trait FSlickQuery {

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

}*/ 