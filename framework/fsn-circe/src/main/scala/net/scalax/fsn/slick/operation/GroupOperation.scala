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

  val groupShape: Shape[_ <: FlatShapeLevel, Rep[Option[RepDataType]], Option[RepDataType], Rep[Option[RepDataType]]]
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
      FAtomicQuery(needAtomicOpt[GroupSlickSelect] :: needAtomicOpt[GroupableColumnBase] :: needAtomicOpt[CountableGroupColumn] :: needAtomic[FProperty] :: HNil)
        .mapToOption(path) {
          case (selectOpt :: groupColOpt :: countOpt :: property :: HNil, data) => {
            val aa = (selectOpt, groupColOpt, countOpt) match {
              case (Some(t), None, None) =>
                new GroupSlickReader {

                  override val propertyName = property.proName
                  override type BaseDataType = path.DataType
                  override val selectModel = t

                  override val groupModel = Option.empty[GroupWraperBase[selectModel.TargetType]]

                }
              case (None, Some(t), None) =>
                new GroupSlickReader {

                  override val propertyName = property.proName
                  override type BaseDataType = t.selectModel.DataType
                  override val selectModel = t.selectModel

                  override val groupModel = Option {
                    t match {
                      case a: GroupableNoOptionColumn[path.DataType] =>
                        new GroupWraperWithNonOption[selectModel.TargetType] {
                          override type RepDataType = a.RepType

                          override val groupShape = a.groupModel
                          override val colToOrder = a.colToOrder
                          override val baseTypedType = a.baseTypedType
                          override val typedType = a.typedType
                          override val targetColConvert = a.targetColConvert.asInstanceOf[t.TargetType => Rep[RepDataType]]
                        }
                      case a: GroupableOptionColumn[path.DataType] =>
                        new GroupWraperWithOption[selectModel.TargetType] {
                          override type RepDataType = a.RepType

                          override val groupShape = a.groupModel
                          override val colToOrder = a.colToOrder
                          override val baseTypedType = a.baseTypedType
                          override val typedType = a.typedType
                          override val targetColConvert = a.targetColConvert.asInstanceOf[t.TargetType => Rep[Option[RepDataType]]]
                        }
                    }
                  }

                }
              case (None, None, Some(t)) =>
                new GroupSlickReader {

                  override val propertyName = property.proName
                  override type BaseDataType = t.selectModel.DataType
                  override val selectModel = t.selectModel

                  override val groupModel = Option.empty[GroupWraperBase[selectModel.TargetType]]

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
    intTyped: BaseTypedType[Int],
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
            case ("count", reader) =>
              val eachValue = queries.map { values =>
                values(reader._2).asInstanceOf[reader._1.selectModel.TargetType]
              }(reader._1.selectModel.shape.packedShape)
              eachValue.length
            case (method, reader) =>
              val helper = new ExtensionMethodConversions {}
              val groupModel = reader._1.groupModel
              groupModel match {
                case Some(a: GroupWraperWithOption[reader._1.selectModel.TargetType]) =>
                  val eachValue = queries.map { values =>
                    a.targetColConvert(values(reader._2).asInstanceOf[a.TargetType])
                  }(a.groupShape)
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
              }
          }
    } {
      val aa = keyIndexs.map(index => readers(index)._1.selectModel.shape.packedShape.asInstanceOf[Shape[FlatShapeLevel, Any, Any, Any]])
      val bb = aggregateIndexsAndMethods.map {
        case ("count", reader) =>
          Shape.repColumnShape[Int, FlatShapeLevel](intTyped)
        //reader._1.groupModel.get.groupShape.asInstanceOf[Shape[FlatShapeLevel, Any, Any, Any]]
        case (_, reader) =>
          reader._1.groupModel.get.groupShape.asInstanceOf[Shape[FlatShapeLevel, Any, Any, Any]]
      }
      new ListColumnShape[FlatShapeLevel](aa ::: bb)
    }

    val action = jsonEv(resultQuery.to[List]).result.map { s =>
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