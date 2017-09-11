package net.scalax.ubw.slick.operation

import net.scalax.ubw.common.atomic.DefaultValue
import net.scalax.ubw.core._
import net.scalax.ubw.json.operation.{AtomicValueHelper, FSomeValue}
import net.scalax.ubw.slick.atomic.{OneToOneUpdate, SlickUpdate}
import net.scalax.ubw.slick.helpers.{FilterColumnGen, ListAnyShape, SlickQueryBindImpl}
import net.scalax.ubw.slick.operation.InCreateConvert.CreateType
import slick.jdbc.JdbcProfile

import scala.concurrent.ExecutionContext
import shapeless._
import slick.lifted._

case class DataWithIndex(data: AtomicValue, index: Int)
case class ExecInfo3[T](effectRows: Int, columns: T)
/*case class ExecInfo4(effectRows: Int, content: DataPileContent) {
  lazy val extractContent: ExtractContent = ExtractContent(content)
  def extract[E](extractor: Extractor[E]): Option[E] = {
    extractContent.extract(extractor)
  }

  def extractAnyway[E](extractor: Extractor[E]): E = {
    extractContent.extractAnyway(extractor)
  }
}*/
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
  /*type UpdateType[T] = List[(Any, SlickQueryBindImpl)] => slick.dbio.DBIO[ExecInfo3[T]]

  def functor(implicit ec: ExecutionContext): Functor[UpdateType] = new Functor[UpdateType] {
    override def map[A, B](fa: UpdateType[A])(f: (A) => B): UpdateType[B] = {
      { binds: List[(Any, SlickQueryBindImpl)] =>
        fa(binds).map(s => ExecInfo3(s.effectRows, f(s.columns)))
      }
    }
  }*/
  def updateGen(
    implicit
    slickProfile: JdbcProfile,
    ec: ExecutionContext
  ): SingleFoldableChannel[CreateType[DataPileContent], CreateType] = {
    DataPile.transformTree(
      new AtomicQuery(_) {
        val aa = withRep(needAtomic[SlickUpdate] :: needAtomicOpt[OneToOneUpdate] :: needAtomicOpt[DefaultValue] :: FANil)
          .mapTo {
            case (slickWriter :: oneToOneUpdateOpt :: defaultOpt :: HNil, data) => { () =>
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
      }.aa/*, ValidatorOperation.readValidator*/) { (genList, atomicValueGen) =>
      CreateType { binds: List[(Any, SlickQueryBindImpl)] =>
        val genListWithData = genList.zipWithIndex.map {
          case (s, index) =>
            val writer1 = s()
            new ISlickUpdaterWithData {
              override val writer = writer1
              override val data = DataWithIndex(set(writer1.data), index)
            }
        }
        UpdateOperation.parseInsert(binds, genListWithData).map { s =>
          ExecInfo3(s.effectRows, atomicValueGen.toContent(s.columns.sortBy(_.index).map(_.data)))
        }
      }
    }.withSyntax(new PileSyntaxFunctor[CreateType[DataPileContent], CreateType] {
      override def pileMap[U](a: CreateType[DataPileContent], pervious: DataPileContent => U): CreateType[U] = {
        CreateType { binds: List[(Any, SlickQueryBindImpl)] =>
          a(binds).map { execInfo =>
            ExecInfo3(execInfo.effectRows, pervious(execInfo.columns))
          }
        }
      }
    }).withFunctor(InCreateConvert.functor)
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
  ): slickProfile.api.DBIO[ExecInfo3[List[DataWithIndex]]] = {
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
            ExecInfo3[List[DataWithIndex]](effectRows + subResult.effectRows, data ::: subResult.columns)
          }
      }
      results.foldLeft(DBIO.successful(ExecInfo3[List[DataWithIndex]](0, Nil)): DBIO[ExecInfo3[List[DataWithIndex]]]) { (s, t) =>
        (for {
          s1 <- s
          t1 <- t
        } yield {
          ExecInfo3[List[DataWithIndex]](s1.effectRows + t1.effectRows, s1.columns ::: t1.columns)
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
  ): slickProfile.api.DBIO[ExecInfo3[List[DataWithIndex]]] = {
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
            ExecInfo3[List[DataWithIndex]](effectRows + subResult.effectRows, subResult.columns ::: data)
          }
      }
      results.foldLeft(DBIO.successful(ExecInfo3[List[DataWithIndex]](0, Nil)): DBIO[ExecInfo3[List[DataWithIndex]]]) { (s, t) =>
        (for {
          s1 <- s
          t1 <- t
        } yield {
          ExecInfo3[List[DataWithIndex]](s1.effectRows + t1.effectRows, s1.columns ::: t1.columns)
        })
      }
    } catch {
      case e: Exception =>
        DBIO.failed(e)
    }
  }

}