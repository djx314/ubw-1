package net.scalax.fsn.slick.operation

import net.scalax.fsn.core._
import net.scalax.fsn.json.operation.FAtomicValueHelper
import net.scalax.fsn.slick.atomic.{ AutoInc, OneToOneCrate, SlickCreate }
import net.scalax.fsn.slick.helpers.{ ListAnyShape, SlickQueryBindImpl, SlickUtils }
import slick.dbio.DBIO
import slick.jdbc.JdbcActionComponent
import slick.lifted.{ FlatShapeLevel, Query, Shape }
import shapeless._

import scala.concurrent.ExecutionContext

trait ISlickWriterWithData {
  val writer: ISlickWriter
  val dataGen: writer.IncValue => DataWithIndex
}

trait InsertDataQuery {

  val bind: SlickQueryBindImpl
  val cols: List[Any]
  val shapes: List[Shape[_ <: FlatShapeLevel, _, _, _]]
  val data: List[Any]
  val returningCols: List[Any]
  val returningShapes: List[Shape[_ <: FlatShapeLevel, _, _, _]]

  def dataGen(returningData: List[Any]): List[DataWithIndex]

}

trait InsertWrapTran[I] {
  val table: Any
  def convert(inc: I, source: InsertDataQuery): InsertDataQuery
}

trait ISlickWriter {
  type PreRep
  type PreValue
  type PreTarget
  type IncRep
  type IncValue
  type IncTarget

  val preData: PreValue
  val table: Any
  val preRep: PreRep
  val preShape: Shape[_ <: FlatShapeLevel, PreRep, PreValue, PreTarget]
  val autoIncRep: IncRep
  val autoIncShape: Shape[_ <: FlatShapeLevel, IncRep, IncValue, IncTarget]
  val subGen: Option[InsertWrapTran[IncValue]]
  val autalColumn: IncValue => Any
}

case class ISWriter[A, B, C, D, E, F](
    override val preData: B,
    override val table: Any,
    override val preRep: A,
    override val preShape: Shape[_ <: FlatShapeLevel, A, B, C],
    override val autoIncRep: D,
    override val autoIncShape: Shape[_ <: FlatShapeLevel, D, E, F],
    override val subGen: Option[InsertWrapTran[E]],
    override val autalColumn: E => Any
) extends ISlickWriter {
  override type PreRep = A
  override type PreValue = B
  override type PreTarget = C
  override type IncRep = D
  override type IncValue = E
  override type IncTarget = F
}

object InCreateConvert extends FAtomicValueHelper {

  def createGen(
    implicit
    ec: ExecutionContext,
    cv: Query[_, Seq[Any], Seq] => JdbcActionComponent#InsertActionExtensionMethods[Seq[Any]],
    retrieveCv: Query[_, Seq[Any], Seq] => JdbcActionComponent#StreamingQueryActionExtensionMethods[Seq[Seq[Any]], Seq[Any]]
  ): FPileSyntax.PileGen[List[(Any, SlickQueryBindImpl)] => DBIO[ExecInfo3]] = {
    FPile.transformTreeList {
      new FAtomicQuery(_) {
        val aa = withRep(needAtomic[SlickCreate] :: needAtomicOpt[AutoInc] :: needAtomicOpt[OneToOneCrate] :: FANil)
          .mapTo {
            case (slickCreate :: autoIncOpt :: oneToOneCreateOpt :: HNil, data) => {
              //val slickWriter = FColumn.find(columns)({ case s: SlickUpdate[columns.DataType] => s })
              //val oneToOneUpdateOpt = FColumn.findOpt(columns)({ case s: OneToOneUpdate[columns.DataType] => s })
              val isAutoInc = autoIncOpt.map(_.isAutoInc).getOrElse(false)
              val writer = if (isAutoInc) {
                lazy val oneToOneSubGen = oneToOneCreateOpt.map { oneToOneCreate =>
                  new InsertWrapTran[slickCreate.SlickType] {
                    override val table = oneToOneCreate.owner
                    def convert(sourceData: slickCreate.SlickType, source: InsertDataQuery): InsertDataQuery = {
                      new InsertDataQuery {
                        override val bind = source.bind
                        override val cols = source.cols ::: oneToOneCreate.mainCol :: Nil
                        override val shapes = source.shapes ::: oneToOneCreate.mainShape :: Nil
                        override val data = source.data ::: oneToOneCreate.convert(slickCreate.convert(sourceData)) :: Nil
                        override val returningCols = source.returningCols
                        override val returningShapes = source.returningShapes
                        override def dataGen(returningData: List[Any]): List[DataWithIndex] = source.dataGen(returningData)
                      }
                    }
                  }
                }

                ISWriter(
                  preData = (),
                  table = slickCreate.owner,
                  preRep = (),
                  preShape = implicitly[Shape[FlatShapeLevel, Unit, Unit, Unit]],
                  autoIncRep = (slickCreate.mainCol: slickCreate.SourceType),
                  autoIncShape = slickCreate.mainShape,
                  subGen = oneToOneSubGen,
                  autalColumn = (s: slickCreate.SlickType) => {
                  slickCreate.convert(s)
                }
                )
              } else {
                lazy val oneToOneSubGen = oneToOneCreateOpt.map { oneToOneCreate =>
                  new InsertWrapTran[Unit] {
                    override val table = oneToOneCreate.owner
                    def convert(sourceData: Unit, source: InsertDataQuery): InsertDataQuery = {
                      val commonData = data
                      new InsertDataQuery {
                        override val bind = source.bind
                        override val cols = source.cols ::: oneToOneCreate.mainCol :: Nil
                        override val shapes = source.shapes ::: oneToOneCreate.mainShape :: Nil
                        override val data = source.data ::: oneToOneCreate.convert(commonData.get) :: Nil
                        override val returningCols = source.returningCols
                        override val returningShapes = source.returningShapes
                        override def dataGen(returningData: List[Any]): List[DataWithIndex] = source.dataGen(returningData)
                      }
                    }
                  }
                }

                ISWriter(
                  preData = {
                  slickCreate.reverseConvert(data.get)
                },
                  table = slickCreate.owner,
                  preRep = slickCreate.mainCol,
                  preShape = slickCreate.mainShape,
                  autoIncRep = (),
                  autoIncShape = implicitly[Shape[FlatShapeLevel, Unit, Unit, Unit]],
                  subGen = oneToOneSubGen,
                  autalColumn = (s: Unit) => data.get
                )
              }
              writer: ISlickWriter
            }
          }
      }.aa
    } { genList =>
      { binds: List[(Any, SlickQueryBindImpl)] =>
        val genListWithIndex = genList.zipWithIndex.map {
          case (gen, index) =>
            new ISlickWriterWithData {
              override val writer = gen
              override val dataGen = { s: writer.IncValue =>
                DataWithIndex(set(writer.autalColumn(s)), index)
              }
            }
        }
        CreateOperation.parseInsert(binds, genListWithIndex)
      }
    }
  }

}

object CreateOperation {

  trait InsWrapTran2 {
    val table: Any
    def convert(source: InsertDataQuery): InsertDataQuery
  }

  def parseInsertGen(
    binds: List[(Any, SlickQueryBindImpl)],
    insertList: List[ISlickWriterWithData],
    //autoIncCols: List[ISlickWriterWithData],
    converts: List[InsWrapTran2]
  )(
    implicit
    ec: ExecutionContext,
    cv: Query[_, Seq[Any], Seq] => JdbcActionComponent#InsertActionExtensionMethods[Seq[Any]],
    retrieveCv: Query[_, Seq[Any], Seq] => JdbcActionComponent#StreamingQueryActionExtensionMethods[Seq[Seq[Any]], Seq[Any]]
  ): DBIO[ExecInfo3] = try {
    val wrapList = insertList

    val currents = wrapList.groupBy(_.writer.table).filter { case (key, s) => converts.exists(t => key == t.table) }
    val results = currents.map {
      case (table, eachWrap) =>
        val initCreateQuery: InsertDataQuery = new InsertDataQuery {
          override val bind = binds.find(_._1 == table).get._2
          override val cols = eachWrap.map(_.writer.preRep)
          override val shapes = eachWrap.map(_.writer.preShape)
          override val data = eachWrap.map(_.writer.preData)
          override val returningCols = eachWrap.map(_.writer.autoIncRep)
          override val returningShapes = eachWrap.map(_.writer.autoIncShape)
          override def dataGen(returningData: List[Any]): List[DataWithIndex] = eachWrap.zip(returningData).map {
            case (wrap, data) =>
              wrap.dataGen(data.asInstanceOf[wrap.writer.IncValue])
          }
        }
        val convertRetrieveQuery = converts.filter(_.table == table).foldLeft(initCreateQuery) { (x, y) =>
          y.convert(x)
        }
        val query = Query(convertRetrieveQuery.cols)(new ListAnyShape[FlatShapeLevel](convertRetrieveQuery.shapes))
        val bindQuery = convertRetrieveQuery.bind.bind(query)
        val returningShape = new ListAnyShape[FlatShapeLevel](convertRetrieveQuery.returningShapes)
        val returingQuery = Query(convertRetrieveQuery.returningCols)(returningShape)
        val incDataDBIO = if (SlickUtils.isShapeEmpty(returningShape)) {
          (bindQuery += convertRetrieveQuery.data) >> returingQuery.result.head
        } else {
          val bindReturingQuery = convertRetrieveQuery.bind.bind(returingQuery)
          val createQuery = bindQuery returning bindReturingQuery
          createQuery += convertRetrieveQuery.data
        }
        for {
          incData <- incDataDBIO
          fillSubGens = eachWrap.zip(incData).map {
            case (wrap, dataItem) =>
              val wrapSlickData = dataItem.asInstanceOf[wrap.writer.IncValue]
              val subGens = wrap.writer.subGen.map { gen =>
                new InsWrapTran2 {
                  override val table = gen.table
                  override def convert(source: InsertDataQuery): InsertDataQuery = {
                    gen.convert(wrapSlickData, source)
                  }
                }
              }
              subGens //-> wrap.autalColumn(wrapSlickData)
          }
          subResult <- parseInsertGen(binds, insertList, fillSubGens.flatten)
        } yield {
          ExecInfo3(subResult.effectRows + 1, convertRetrieveQuery.dataGen(incData.toList) ::: subResult.columns)
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

  def parseInsert(
    binds: List[(Any, SlickQueryBindImpl)],
    insertList: List[ISlickWriterWithData]
  )(
    implicit
    ec: ExecutionContext,
    cv: Query[_, Seq[Any], Seq] => JdbcActionComponent#InsertActionExtensionMethods[Seq[Any]],
    retrieveCv: Query[_, Seq[Any], Seq] => JdbcActionComponent#StreamingQueryActionExtensionMethods[Seq[Seq[Any]], Seq[Any]]
  ): DBIO[ExecInfo3] = try {
    val wrapList = insertList //.map(InCreateConvert2.convert)

    val subGensTables = wrapList.flatMap { t => t.writer.subGen.toList.map(_.table) }
    val currents = wrapList.groupBy(_.writer.table).filter { case (key, s) => subGensTables.forall(t => key != t) }
    val results = currents.map {
      case (table, eachWrap) =>
        val initCreateQuery: InsertDataQuery = new InsertDataQuery {
          override val bind = binds.find(_._1 == table).get._2
          override val cols = eachWrap.map(_.writer.preRep)
          override val shapes = eachWrap.map(_.writer.preShape)
          override val data = eachWrap.map(_.writer.preData)
          override val returningCols = eachWrap.map(_.writer.autoIncRep)
          override val returningShapes = eachWrap.map(_.writer.autoIncShape)
          override def dataGen(returningData: List[Any]): List[DataWithIndex] = eachWrap.zip(returningData).map {
            case (wrap, data) =>
              wrap.dataGen(data.asInstanceOf[wrap.writer.IncValue])
          }
        }
        val convertRetrieveQuery = initCreateQuery
        val query = Query(convertRetrieveQuery.cols)(new ListAnyShape[FlatShapeLevel](convertRetrieveQuery.shapes))
        val bindQuery = convertRetrieveQuery.bind.bind(query)
        val returningShape = new ListAnyShape[FlatShapeLevel](convertRetrieveQuery.returningShapes)
        val returingQuery = Query(convertRetrieveQuery.returningCols)(returningShape)
        val incDataDBIO = if (SlickUtils.isShapeEmpty(returningShape)) {
          (bindQuery += convertRetrieveQuery.data) >> returingQuery.result.head
        } else {
          val bindReturingQuery = convertRetrieveQuery.bind.bind(returingQuery)
          val createQuery = bindQuery returning bindReturingQuery
          createQuery += convertRetrieveQuery.data
        }
        for {
          incData <- incDataDBIO
          fillSubGens = eachWrap.zip(incData).map {
            case (wrap, dataItem) =>
              val wrapSlickData = dataItem.asInstanceOf[wrap.writer.IncValue]
              val subGens = wrap.writer.subGen.map { gen =>
                new InsWrapTran2 {
                  override val table = gen.table
                  override def convert(source: InsertDataQuery): InsertDataQuery = {
                    gen.convert(wrapSlickData, source)
                  }
                }
              }
              subGens
          }
          subResult <- parseInsertGen(binds, insertList, fillSubGens.flatten)
        } yield {
          ExecInfo3(subResult.effectRows + 1, convertRetrieveQuery.dataGen(incData.toList) ::: subResult.columns)
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