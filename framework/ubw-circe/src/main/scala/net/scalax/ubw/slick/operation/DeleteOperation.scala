package net.scalax.fsn.slick.operation

import net.scalax.fsn.core._
import net.scalax.fsn.json.operation.{ FAtomicValueHelper, FSomeValue }
import net.scalax.fsn.slick.atomic.{ OneToOneRetrieve, SlickDelete }
import net.scalax.fsn.slick.helpers.{ FilterColumnGen, ListAnyShape, SlickQueryBindImpl }
import slick.dbio.DBIO
import slick.jdbc.JdbcActionComponent
import slick.lifted._
import slick.relational.RelationalProfile
import shapeless._

import scala.concurrent.ExecutionContext

trait DeleteTran2 {
  val table: Any
  def convert(source: DeleteQuery): DeleteQuery
}

trait DeleteQuery {

  val bind: SlickQueryBindImpl
  val cols: List[Any]
  val shapes: List[Shape[_ <: FlatShapeLevel, _, _, _]]
  val filters: List[FilterColumnGen[Seq[Any]]]

}

trait DSlickWriter2 {

  type MainSColumn
  //type MainDColumn
  type MainTColumn
  type DataType

  val mainCol: MainSColumn

  //val mainShape: Shape[_ <: FlatShapeLevel, MainSColumn, MainDColumn, MainTColumn]
  val mainShape: Shape[_ <: FlatShapeLevel, MainSColumn, DataType, MainTColumn]

  val table: Any
  val primaryGen: Option[FilterColumnGen[MainTColumn]]

  val data: DataType

  val subGen: Option[DeleteTran2]

}

case class DSWriter2[MS /*, MD*/ , MT, D](
    override val mainCol: MS,
    override val mainShape: Shape[_ <: FlatShapeLevel, MS, D, MT],
    override val table: Any,
    override val primaryGen: Option[FilterColumnGen[MT]],
    override val data: D,
    override val subGen: Option[DeleteTran2]
) extends DSlickWriter2 {

  override type MainSColumn = MS
  //override type MainDColumn = MD
  override type MainTColumn = MT
  override type DataType = D

}

trait ISlickDeleteWithData {
  val writer: DSlickWriter2
  val data: DataWithIndex
}

object InDeleteConvert extends FAtomicValueHelper {
  def convert(
    implicit
    ec: ExecutionContext,
    deleteConV: Query[RelationalProfile#Table[_], _, Seq] => JdbcActionComponent#DeleteActionExtensionMethods
  ) = {
    FPile.transformTreeList {
      new FAtomicQuery(_) {
        val aa = withRep(needAtomic[SlickDelete] :: needAtomicOpt[OneToOneRetrieve] :: FANil)
          .mapTo {
            case (slickDelete :: oneToDeleteOpt :: HNil, data) =>
              val subGenOpt = oneToDeleteOpt.map { oneToOneDelete =>
                new DeleteTran2 {
                  override val table = oneToOneDelete.owner

                  override def convert(source: DeleteQuery): DeleteQuery = {
                    new DeleteQuery {
                      override val bind = source.bind
                      override val cols = source.cols ::: oneToOneDelete.mainCol :: Nil
                      override val shapes = source.shapes ::: oneToOneDelete.mainShape :: Nil
                      override val filters = source.filters ::: {
                        val index = cols.indexOf(oneToOneDelete.mainCol)
                        new FilterColumnGen[Seq[Any]] {
                          override type BooleanTypeRep = oneToOneDelete.primaryGen.BooleanTypeRep
                          override val dataToCondition = { cols: Seq[Any] =>
                            val col = cols(index).asInstanceOf[oneToOneDelete.TargetType]
                            val FSomeValue(data1) = data
                            val slickData = data1 //oneToOneDelete.filterConvert(data1)
                            oneToOneDelete.primaryGen.dataToCondition(col)(slickData)
                          }
                          override val wt = oneToOneDelete.primaryGen.wt
                        }
                      } :: Nil
                    }
                  }
                }
              }
              DSWriter2(
                mainCol = slickDelete.mainCol,
                mainShape = slickDelete.mainShape,
                table = slickDelete.owner,
                primaryGen = slickDelete.primaryGen.map { eachPri =>
                  (new FilterColumnGen[slickDelete.TargetType] {
                    override type BooleanTypeRep = eachPri.BooleanTypeRep
                    override val dataToCondition = { sourceCol: slickDelete.TargetType =>
                      eachPri.dataToCondition(sourceCol) {
                        val FSomeValue(data1) = data
                        //slickDelete.filterConvert(data1)
                        data1
                      }
                    }
                    override val wt = eachPri.wt
                  })
                },
                data = {
                  val FSomeValue(data1) = data
                  data1
                },
                subGen = subGenOpt
              ): DSlickWriter2
          }
      }.aa
    } { genList =>
      { binds: List[(Any, SlickQueryBindImpl)] =>
        val genListWithData = genList.zipWithIndex.map {
          case (gen, index) =>
            new ISlickDeleteWithData {
              override val writer = gen
              override val data = DataWithIndex(set(writer.data), index)
            }
        }
        DeleteOperation.parseInsert(binds, genListWithData)
      }
    }
  }
}

object DeleteOperation {

  def parseInsertGen(
    binds: List[(Any, SlickQueryBindImpl)],
    updateList: List[ISlickDeleteWithData],
    converts: List[DeleteTran2]
  )(
    implicit
    ec: ExecutionContext,
    deleteConV: Query[RelationalProfile#Table[_], _, Seq] => JdbcActionComponent#DeleteActionExtensionMethods
  ): DBIO[ExecInfo3] = {
    val wrapList = updateList

    val currents = wrapList.groupBy(_.writer.table).filter { case (key, s) => converts.exists(t => key == t.table) }
    val results = currents.map {
      case (table, eachWrap) =>
        val initDeleteQuery: DeleteQuery = new DeleteQuery {
          override val bind = binds.find(_._1 == table).get._2
          override val cols = eachWrap.map(_.writer.mainCol)
          override val shapes = eachWrap.map(_.writer.mainShape)
          override val filters = eachWrap.zipWithIndex.map {
            case (gen, index) =>
              gen.writer.primaryGen.map { priGen =>
                new FilterColumnGen[Seq[Any]] {
                  override type BooleanTypeRep = priGen.BooleanTypeRep
                  override val dataToCondition = { cols: Seq[Any] =>
                    priGen.dataToCondition(cols(index).asInstanceOf[gen.writer.MainTColumn])
                  }
                  override val wt = priGen.wt
                }
              }.toList: List[FilterColumnGen[Seq[Any]]]
          }.flatten
        }

        val data = eachWrap.map(_.data)
        val convertRetrieveQuery = converts.filter(_.table == table).foldLeft(initDeleteQuery) { (x, y) =>
          y.convert(x)
        }
        val query = Query(convertRetrieveQuery.cols)(new ListAnyShape[FlatShapeLevel](convertRetrieveQuery.shapes))
        val bindQuery = convertRetrieveQuery.bind.bind(query)
        val filterQuery = convertRetrieveQuery.filters.foldLeft(bindQuery) { (x, y) =>
          x.filter(s => y.dataToCondition(s))(y.wt)
        }
        val updateDBIO = filterQuery.asInstanceOf[Query[RelationalProfile#Table[_], _, Seq]].delete
        for {
          effectRows <- updateDBIO
          subs = eachWrap.map(_.writer.subGen.toList).flatten
          subResult <- parseInsertGen(binds, updateList, subs)
        } yield {
          ExecInfo3(effectRows + subResult.effectRows, data ::: subResult.columns)
        }
    }
    results.foldLeft(DBIO.successful(ExecInfo3(0, Nil)): DBIO[ExecInfo3]) { (s, t) =>
      (for {
        s1 <- s
        t1 <- t
      } yield {
        ExecInfo3(s1.effectRows + t1.effectRows, s1.columns ::: t1.columns)
      })
    }
  }

  def parseInsert(
    binds: List[(Any, SlickQueryBindImpl)],
    updateList: List[ISlickDeleteWithData]
  )(
    implicit
    ec: ExecutionContext,
    deleteConV: Query[RelationalProfile#Table[_], _, Seq] => JdbcActionComponent#DeleteActionExtensionMethods
  ): DBIO[ExecInfo3] = {
    val wrapList = updateList

    val subGensTables = wrapList.flatMap { t => t.writer.subGen.toList.map(_.table) }
    val currents = wrapList.groupBy(_.writer.table).filter { case (key, s) => subGensTables.forall(t => key != t) }
    val results = currents.map {
      case (table, eachWrap) =>
        val initDeleteQuery: DeleteQuery = new DeleteQuery {
          override val bind = binds.find(_._1 == table).get._2
          override val cols = eachWrap.map(_.writer.mainCol)
          override val shapes = eachWrap.map(_.writer.mainShape)
          override val filters = eachWrap.zipWithIndex.map {
            case (gen, index) =>
              gen.writer.primaryGen.map { priGen =>
                new FilterColumnGen[Seq[Any]] {
                  override type BooleanTypeRep = priGen.BooleanTypeRep
                  override val dataToCondition = { cols: Seq[Any] =>
                    priGen.dataToCondition(cols(index).asInstanceOf[gen.writer.MainTColumn])
                  }
                  override val wt = priGen.wt
                }
              }.toList: List[FilterColumnGen[Seq[Any]]]
          }.flatten
        }

        val data = eachWrap.map(_.data)
        val convertRetrieveQuery = initDeleteQuery
        val query = Query(convertRetrieveQuery.cols)(new ListAnyShape[FlatShapeLevel](convertRetrieveQuery.shapes))
        val bindQuery = convertRetrieveQuery.bind.bind(query)
        val filterQuery = convertRetrieveQuery.filters.foldLeft(bindQuery) { (x, y) =>
          x.filter(s => y.dataToCondition(s))(y.wt)
        }
        val updateDBIO = filterQuery.asInstanceOf[Query[RelationalProfile#Table[_], _, Seq]].delete
        for {
          effectRows <- updateDBIO
          subs = eachWrap.map(_.writer.subGen.toList).flatten
          subResult <- parseInsertGen(binds, updateList, subs)
        } yield {
          ExecInfo3(effectRows + subResult.effectRows, data ::: subResult.columns)
        }
    }
    results.foldLeft(DBIO.successful(ExecInfo3(0, Nil)): DBIO[ExecInfo3]) { (s, t) =>
      (for {
        s1 <- s
        t1 <- t
      } yield {
        ExecInfo3(s1.effectRows + t1.effectRows, s1.columns ::: t1.columns)
      })
    }
  }
}