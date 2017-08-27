package net.scalax.fsn.slick.operation

import net.scalax.fsn.common.atomic.DefaultValue
import net.scalax.fsn.core._
import net.scalax.fsn.json.operation.{AtomicValueHelper, FSomeValue, ValidatorOperation}
import net.scalax.fsn.slick.atomic.{OneToOneUpdate, SlickUpdate}
import net.scalax.fsn.slick.helpers.{FilterColumnGen, ListAnyShape, SlickQueryBindImpl}
import net.scalax.ubw.validate.atomic.ErrorMessage
import slick.jdbc.JdbcProfile

import scala.concurrent.{ExecutionContext, Future}
import shapeless._
import slick.lifted._

case class DataWithIndex(data: AtomicValue, index: Int)
case class ExecInfo3(effectRows: Int, columns: List[DataWithIndex])

trait UpdateTran {
  val table: Any
  def convert(source: UpdateQuery): UpdateQuery
}

trait UpdateQuery {

  val bind: SlickQueryBindImpl
  val cols: List[Any]
  val shapes: List[Shape[_ <: FlatShapeLevel, _, _, _]]
  val filters: List[FilterColumnGen[Seq[Any]]]

  val updateIndices: List[Int]
  val updateShapes: List[Shape[_ <: FlatShapeLevel, _, _, _]]
  val updateData: List[Any]

}

trait USlickWriter {

  type MainSColumn
  type MainDColumn
  type MainTColumn

  val mainCol: MainSColumn

  val mainShape: Shape[_ <: FlatShapeLevel, MainSColumn, MainDColumn, MainTColumn]

  val table: Any

  val data: MainDColumn

  val primaryGen: Option[FilterColumnGen[MainTColumn]]

  val subGen: Option[UpdateTran]

}

case class USWriter[MS, MD, MT](
    override val mainCol: MS,
    override val mainShape: Shape[_ <: FlatShapeLevel, MS, MD, MT],
    override val table: Any,
    override val data: MD,
    override val primaryGen: Option[FilterColumnGen[MT]],
    override val subGen: Option[UpdateTran]
) extends USlickWriter {

  override type MainSColumn = MS
  override type MainDColumn = MD
  override type MainTColumn = MT

}

trait ISlickUpdaterWithData {
  val writer: USlickWriter
  val data: DataWithIndex
}

object InUpdateConvert extends AtomicValueHelper {

  def updateGen(
    implicit
    slickProfile: JdbcProfile,
    ec: ExecutionContext
  ): PileSyntax.PileGen[(List[(Any, SlickQueryBindImpl)] => Future[slickProfile.api.DBIO[ExecInfo3]], Future[List[ErrorMessage]])] = {
    Pile.transformTreeListWithFilter({
      new AtomicQuery(_) {
        val aa = withRep(needAtomic[SlickUpdate] :: needAtomicOpt[OneToOneUpdate] :: needAtomicOpt[DefaultValue] :: FANil)
          .mapTo {
            case (slickWriter :: oneToOneUpdateOpt :: defaultOpt :: HNil, data) => {
              val uSlickSubGen = oneToOneUpdateOpt.map { oneToOneUpdate =>
                new UpdateTran {
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
                            val FSomeValue(data1) = data
                            val slickData = data1 //oneToOneUpdate.filterConvert(data1)
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

              val uSlickWriter = USWriter(
                mainCol = slickWriter.mainCol,
                mainShape = slickWriter.mainShape,
                table = slickWriter.owner,
                data = {
                  data match {
                    case FSomeValue(data1) => data1
                    case _ => defaultOpt.map(_.value).get
                  }
              },
                primaryGen = slickWriter.primaryGen.map { eachPri =>
                  (new FilterColumnGen[slickWriter.TargetType] {
                    override type BooleanTypeRep = eachPri.BooleanTypeRep
                    override val dataToCondition = { sourceCol: slickWriter.TargetType =>
                      eachPri.dataToCondition(sourceCol) {
                        data match {
                          case FSomeValue(data1) => data1
                          case _ => defaultOpt.map(_.value).get
                        }
                      }
                    }
                    override val wt = eachPri.wt
                  })
                },
                subGen = uSlickSubGen
              )
              uSlickWriter: USlickWriter
            }
          }
      }.aa
    }, ValidatorOperation.readValidator)({ genListF =>
      { binds: List[(Any, SlickQueryBindImpl)] =>
        genListF.map { genList =>
          val genListWithData = genList.zipWithIndex.map {
            case (s, index) =>
              new ISlickUpdaterWithData {
                override val writer = s
                override val data = DataWithIndex(set(writer.data), index)
              }
          }
          UpdateOperation.parseInsert(binds, genListWithData)
        }
      }
    }, { validateIfos =>
      validateIfos.map(_.flatten)
    })
  }
}

object UpdateOperation {

  def parseInsertGen(
    binds: List[(Any, SlickQueryBindImpl)],
    wrapList: List[ISlickUpdaterWithData],
    converts: List[UpdateTran]
  )(
    implicit
    slickProfile: JdbcProfile,
    ec: ExecutionContext
  ): slickProfile.api.DBIO[ExecInfo3] = {
    val profile = slickProfile
    import profile.api._
    try {
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
            subResult <- {
              implicit val _ = profile
              parseInsertGen(binds, wrapList, subs)
            }
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
    } catch {
      case e: Exception =>
        DBIO.failed(e)
    }
  }

  def parseInsert(
    binds: List[(Any, SlickQueryBindImpl)],
    wrapList: List[ISlickUpdaterWithData]
  )(
    implicit
    slickProfile: JdbcProfile,
    ec: ExecutionContext,
  ): slickProfile.api.DBIO[ExecInfo3] = {
    val profile = slickProfile
    import profile.api._
    try {
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
            subResult <- {
              implicit val _ = profile
              parseInsertGen(binds, wrapList, subs)
            }
          } yield {
            ExecInfo3(effectRows + subResult.effectRows, subResult.columns ::: data)
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
    } catch {
      case e: Exception =>
        DBIO.failed(e)
    }
  }

}