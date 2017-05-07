package net.scalax.fsn.slick.operation

import net.scalax.fsn.core._
import net.scalax.fsn.common.atomic.FProperty
import net.scalax.fsn.slick.atomic._
import net.scalax.fsn.slick.helpers.{ ListColumnShape, SlickQueryBindImpl }
import net.scalax.fsn.slick.model._
import shapeless._
import slick.ast.{ BaseTypedType, Ordering, TypedType }
import slick.dbio.DBIO
import slick.jdbc.JdbcActionComponent
import slick.lifted._

import scala.concurrent.ExecutionContext

sealed abstract trait GroupWraperBase {
  type RepDataType

  val groupShape: Shape[_ <: FlatShapeLevel, Rep[Option[RepDataType]], Option[RepDataType], Rep[Option[RepDataType]]]
  val colToOrder: Rep[Option[RepDataType]] => Ordered
  val baseTypedType: BaseTypedType[RepDataType]
  val typedType: TypedType[Option[RepDataType]]

}

trait GroupWraperWithOption extends GroupWraperBase {
  val targetColConvert: Any => Rep[Option[RepDataType]]
}

trait GroupWraperWithNonOption extends GroupWraperBase {
  val targetColConvert: Any => Rep[RepDataType]
}

trait GroupSlickReader {

  val propertyName: String
  val selectModel: GroupSlickSelect[_]
  val groupModel: Option[GroupWraperBase]

}

object GroupSelectConvert {

  def ubwGen(wQuery1: SlickQueryBindImpl): FPileSyntax.PileGen[Option, FGroupQuery] = {
    FPile.transformTreeList {
      new FAtomicQuery(_) {
        val aa = withRep(needAtomic[GroupSlickSelect] :: needAtomicOpt[GroupableColumnBase] :: needAtomicOpt[CountableGroupColumn] :: needAtomic[FProperty] :: FANil)
          .mapToOption {
            case (select :: groupColOpt :: countOpt :: property :: HNil, data) => {
              val aa = (groupColOpt, countOpt) match {
                case (None, None) =>
                  new GroupSlickReader {
                    override val propertyName = property.proName
                    override val selectModel = select
                    override val groupModel = Option.empty[GroupWraperBase]
                  }
                case (Some(t), None) =>
                  new GroupSlickReader {

                    override val propertyName = property.proName
                    override val selectModel = select
                    slick.jdbc.MySQLProfile
                    override val groupModel = Option {
                      t match {
                        case a: GroupableNoOptionColumn[path.DataType] =>
                          new GroupWraperWithNonOption {
                            override type RepDataType = path.DataType
                            override val groupShape = {
                              implicit val typedTypeImplicit = a.baseTypedType
                              implicitly[Shape[FlatShapeLevel, Rep[Option[RepDataType]], Option[RepDataType], Rep[Option[RepDataType]]]]
                            }
                            override val colToOrder = (rep: Rep[Option[RepDataType]]) => ColumnOrdered[Option[RepDataType]](rep, Ordering())
                            override val baseTypedType = a.baseTypedType
                            override val typedType = a.baseTypedType.optionType
                            override val targetColConvert = a.targetColConvert
                          }
                        case a: GroupableOptionColumn[path.DataType] =>
                          new GroupWraperWithOption {
                            override type RepDataType = path.DataType

                            override val groupShape = {
                              implicit val typedTypeImplicit = a.baseTypedType
                              implicitly[Shape[FlatShapeLevel, Rep[Option[RepDataType]], Option[RepDataType], Rep[Option[RepDataType]]]]
                            }
                            override val colToOrder = (rep: Rep[Option[RepDataType]]) => ColumnOrdered[Option[RepDataType]](rep, Ordering())
                            override val baseTypedType = a.baseTypedType
                            override val typedType = a.baseTypedType.optionType
                            override val targetColConvert = a.targetColConvert
                          }
                      }
                    }

                  }
                case (None, Some(t)) =>
                  new GroupSlickReader {

                    override val propertyName = property.proName
                    override val selectModel = select

                    override val groupModel = Option.empty[GroupWraperBase]

                  }
                case _ =>
                  throw new Exception("不可预测的原子集合")
              }
              aa: GroupSlickReader
            }
          }
      }.aa
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
                values(reader._2)
              }(reader._1.selectModel.shape.packedShape.asInstanceOf[Shape[FlatShapeLevel, Any, Any, Any]])
              eachValue.length
            case (method, reader) =>
              val helper = new ExtensionMethodConversions {}
              val groupModel = reader._1.groupModel
              groupModel match {
                case Some(a: GroupWraperWithOption) =>
                  val eachValue = queries.map { values =>
                    a.targetColConvert(values(reader._2))
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
                case Some(a: GroupWraperWithNonOption) =>
                  val eachValue = queries.map { values =>
                    a.targetColConvert(values(reader._2))
                  } {
                    implicit val typedTypeImplicit = a.baseTypedType
                    implicitly[Shape[FlatShapeLevel, Rep[a.RepDataType], a.RepDataType, Rep[a.RepDataType]]]
                  }
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
                case _ =>
                  throw new IllegalArgumentException("You need to provide groupModel when use groupValue")
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

    val sortByInfoMap = aggregateIndexsAndMethods.zipWithIndex.map {
      case ((_, (reader, _)), repIndex) =>
        (reader.propertyName, (keySize + repIndex, reader.groupModel.get.colToOrder.asInstanceOf[Any => ColumnOrdered[_]]))
    }.toMap

    val sortWithKeyInfoMap = keyIndexs.zipWithIndex.map {
      case (keyIndex, repIndex) =>
        val (reader, _) = readers(keyIndex)
        reader.selectModel.colToOrder.map { colToOrder =>
          (reader.propertyName, (repIndex, colToOrder.asInstanceOf[Any => Ordered]))
        }.toList
    }.flatten.toMap

    val orderedQuery = param.orders.foldLeft(resultQuery) { (query, eachOrder) =>
      (sortByInfoMap ++ sortWithKeyInfoMap).get(eachOrder.columnName).map {
        case (index, colToOrder) =>
          query.sortBy { reps =>
            println(reps)
            val orderCol = colToOrder(reps(index))
            if (eachOrder.isDesc) {
              new Ordered(orderCol.columns.map(s => s._1 -> s._2.desc))
            } else {
              new Ordered(orderCol.columns.map(s => s._1 -> s._2.asc))
            }
          }(identity)
      }.getOrElse(query)
    }

    val action = jsonEv(orderedQuery.to[List]).result.map { s =>
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
    GroupResult(action, orderedQuery.to[List].result.statements.toList)
  }
}