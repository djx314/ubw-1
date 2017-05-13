package net.scalax.fsn.slick.operation

import net.scalax.fsn.core._
import net.scalax.fsn.json.operation.FAtomicValueHelper
import net.scalax.fsn.slick.atomic.{ OneToOneUpdate, SlickUpdate }
import net.scalax.fsn.slick.helpers.{ FilterColumnGen, ListAnyShape, SlickQueryBindImpl }
import slick.dbio.DBIO
import slick.jdbc.JdbcActionComponent
import slick.lifted.{ FlatShapeLevel, Query, Shape }

import scala.concurrent.ExecutionContext
import shapeless._

case class DataWithIndex1111(data: FAtomicValue, index: Int)
case class ExecInfo31111(effectRows: Int, columns: List[DataWithIndex1111])

trait ISlickUpdaterWithData1111 {
  val writer: USlickWriter2
  val data: DataWithIndex1111
}

object InUpdateConvert21111 extends FAtomicValueHelper {

  def updateGen(
    implicit
    ec: ExecutionContext,
    updateConV: Query[_, Seq[Any], Seq] => JdbcActionComponent#UpdateActionExtensionMethods[Seq[Any]]
  ): FPileSyntax1111.PileGen[List[(Any, SlickQueryBindImpl)] => DBIO[ExecInfo31111]] = {
    FPile1111.transformTreeList {
      new FAtomicQuery1111(_) {
        val aa = withRep(needAtomic[SlickUpdate] :: needAtomicOpt[OneToOneUpdate] :: FANil)
          .mapTo {
            case (slickWriter :: oneToOneUpdateOpt :: HNil, data) => {
              val uSlickSubGen = oneToOneUpdateOpt.map { oneToOneUpdate =>
                new UUpdateTran2 {
                  override val table = oneToOneUpdate.owner
                  override def convert(source: UpdateQuery): UpdateQuery = {
                    new UpdateQuery {
                      override val bind = source.bind
                      override val cols = source.cols ::: oneToOneUpdate.mainCol :: Nil
                      override val shapes = source.shapes ::: oneToOneUpdate.mainShape :: Nil
                      override val filters = source.filters ::: {
                        val index = cols.indexOf(oneToOneUpdate.mainCol)
                        new FilterColumnGen[Seq[Any]] {
                          override type BooleanTypeRep = oneToOneUpdate.primaryGen.BooleanTypeRep
                          override val dataToCondition = { cols: Seq[Any] =>
                            val col = cols(index).asInstanceOf[oneToOneUpdate.TargetType]
                            val slickData = oneToOneUpdate.filterConvert(data.get)
                            oneToOneUpdate.primaryGen.dataToCondition(col)(slickData)
                          }
                          override val wt = oneToOneUpdate.primaryGen.wt
                        }
                      } :: Nil
                      override val updateIndices = source.updateIndices
                      override val updateShapes = source.updateShapes
                      override val updateData = source.updateData
                    }
                  }
                }
              }

              val uSlickWriter = USWriter2(
                mainCol = slickWriter.mainCol,
                mainShape = slickWriter.mainShape,
                table = slickWriter.owner,
                data = slickWriter.convert(data.get),
                primaryGen = slickWriter.primaryGen.map { eachPri =>
                  (new FilterColumnGen[slickWriter.TargetType] {
                    override type BooleanTypeRep = eachPri.BooleanTypeRep
                    override val dataToCondition = { sourceCol: slickWriter.TargetType =>
                      eachPri.dataToCondition(sourceCol)(
                        slickWriter.filterConvert(data.get)
                      )
                    }
                    override val wt = eachPri.wt
                  })
                },
                subGen = uSlickSubGen
              )
              uSlickWriter: USlickWriter2
            }
          }
      }.aa
    } { genList =>
      { binds: List[(Any, SlickQueryBindImpl)] =>
        val genListWithData = genList.zipWithIndex.map {
          case (s, index) =>
            new ISlickUpdaterWithData1111 {
              override val writer = s
              override val data = DataWithIndex1111(set(writer.data), index)
            }
        }
        UpdateOperation1111.parseInsert(binds, genListWithData)
      }
    }
  }
}

object UpdateOperation1111 {

  def parseInsertGen(
    binds: List[(Any, SlickQueryBindImpl)],
    //updateList: List[FColumn],
    wrapList: List[ISlickUpdaterWithData1111],
    converts: List[UUpdateTran2]
  )(
    implicit
    ec: ExecutionContext,
    updateConV: Query[_, Seq[Any], Seq] => JdbcActionComponent#UpdateActionExtensionMethods[Seq[Any]]
  ): DBIO[ExecInfo31111] = {
    //val wrapList = updateList.map(InUpdateConvert2.convert)

    val currents = wrapList.groupBy(_.writer.table).filter { case (key, s) => converts.exists(t => key == t.table) }
    val results = currents.map {
      case (table, eachWrap) =>
        val initUpdateQuery: UpdateQuery = new UpdateQuery {
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
          override val updateIndices = eachWrap.toStream.zipWithIndex.filter(_._1.writer.primaryGen.isEmpty).map(_._2).toList
          override val updateShapes = eachWrap.toStream.filter(_.writer.primaryGen.isEmpty).map(_.writer.mainShape.packedShape).toList
          override val updateData = eachWrap.toStream.filter(_.writer.primaryGen.isEmpty).map(_.writer.data).toList
        }

        val data = eachWrap.map { s => s.data }
        val convertRetrieveQuery = converts.filter(_.table == table).foldLeft(initUpdateQuery) { (x, y) =>
          y.convert(x)
        }
        val query = Query(convertRetrieveQuery.cols)(new ListAnyShape[FlatShapeLevel](convertRetrieveQuery.shapes))
        val bindQuery = convertRetrieveQuery.bind.bind(query)
        val filterQuery = convertRetrieveQuery.filters.foldLeft(bindQuery) { (x, y) =>
          x.filter(s => y.dataToCondition(s))(y.wt)
        }
        val updateDBIO = filterQuery.map { cols => convertRetrieveQuery.updateIndices.map(index => cols(index)) }(new ListAnyShape[FlatShapeLevel](initUpdateQuery.updateShapes))
          .update(initUpdateQuery.updateData)
        for {
          effectRows <- updateDBIO
          subs = eachWrap.map(_.writer.subGen.toList).flatten
          subResult <- parseInsertGen(binds, wrapList, subs)
        } yield {
          ExecInfo31111(effectRows + subResult.effectRows, data ::: subResult.columns)
        }
    }
    results.foldLeft(DBIO.successful(ExecInfo31111(0, Nil)): DBIO[ExecInfo31111]) { (s, t) =>
      (for {
        s1 <- s
        t1 <- t
      } yield {
        ExecInfo31111(s1.effectRows + t1.effectRows, s1.columns ::: t1.columns)
      })
    }
  }

  def parseInsert(
    binds: List[(Any, SlickQueryBindImpl)],
    wrapList: List[ISlickUpdaterWithData1111]
  )(
    implicit
    ec: ExecutionContext,
    updateConV: Query[_, Seq[Any], Seq] => JdbcActionComponent#UpdateActionExtensionMethods[Seq[Any]]
  ): DBIO[ExecInfo31111] = {
    //val wrapList = updateList.map(InUpdateConvert2.convert)
    val subGensTables = wrapList.flatMap { t => t.writer.subGen.toList.map(_.table) }
    val currents = wrapList.groupBy(_.writer.table).filter { case (key, s) => subGensTables.forall(t => key != t) }
    val results = currents.map {
      case (table, eachWrap) =>
        val initUpdateQuery: UpdateQuery = new UpdateQuery {
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
          override val updateIndices = eachWrap.toStream.zipWithIndex.filter(_._1.writer.primaryGen.isEmpty).map(_._2).toList
          override val updateShapes = eachWrap.toStream.filter(_.writer.primaryGen.isEmpty).map(_.writer.mainShape.packedShape).toList
          override val updateData = eachWrap.toStream.filter(_.writer.primaryGen.isEmpty).map(_.writer.data).toList
        }

        val data = eachWrap.map(_.data)
        val convertRetrieveQuery = initUpdateQuery
        val query = Query(convertRetrieveQuery.cols)(new ListAnyShape[FlatShapeLevel](convertRetrieveQuery.shapes))
        val bindQuery = convertRetrieveQuery.bind.bind(query)
        val filterQuery = convertRetrieveQuery.filters.foldLeft(bindQuery) { (x, y) =>
          x.filter(s => y.dataToCondition(s))(y.wt)
        }
        val updateDBIO = filterQuery.map { cols => convertRetrieveQuery.updateIndices.map(index => cols(index)) }(new ListAnyShape[FlatShapeLevel](initUpdateQuery.updateShapes))
          .update(initUpdateQuery.updateData)
        for {
          effectRows <- updateDBIO
          subs = eachWrap.map(_.writer.subGen.toList).flatten
          subResult <- parseInsertGen(binds, wrapList, subs)
        } yield {
          ExecInfo31111(effectRows + subResult.effectRows, subResult.columns ::: data)
        }
    }
    results.foldLeft(DBIO.successful(ExecInfo31111(0, Nil)): DBIO[ExecInfo31111]) { (s, t) =>
      (for {
        s1 <- s
        t1 <- t
      } yield {
        ExecInfo31111(s1.effectRows + t1.effectRows, s1.columns ::: t1.columns)
      })
    }
  }

}