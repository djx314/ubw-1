package net.scalax.ubw.slick.operation

import cats.Functor
import net.scalax.ubw.common.atomic.{ DefaultValue, FProperty }
import net.scalax.ubw.core._
import net.scalax.ubw.json.operation.{ AtomicValueHelper, FSomeValue }
import net.scalax.ubw.slick.atomic.{ AutoInc, OneToOneCrate, SlickCreate }
import net.scalax.ubw.slick.helpers.{ ListAnyShape, SlickQueryBindImpl, SlickUtils }
import slick.jdbc.JdbcProfile
import shapeless._
import slick.lifted._

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
    override val autoIncShape: slick.lifted.Shape[_ <: slick.lifted.FlatShapeLevel, D, E, F],
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

object InCreateConvert extends AtomicValueHelper {

  type CreateType[T] = List[(Any, SlickQueryBindImpl)] => slick.dbio.DBIO[ExecInfo3[T]]

  def functor(implicit ec: ExecutionContext): Functor[CreateType] = new Functor[CreateType] {
    override def map[A, B](fa: CreateType[A])(f: (A) => B): CreateType[B] = {
      { binds: List[(Any, SlickQueryBindImpl)] =>
        fa(binds).map(s => ExecInfo3(s.effectRows, f(s.columns)))
      }
    }
  }

  def createGen(
    implicit
    slickProfile: JdbcProfile,
    ec: ExecutionContext
  ): FoldableChannel[CreateType[DataPileContent], CreateType] = {
    val profile = slickProfile
    DataPile.transformTree {
      import profile.api._
      new AtomicQuery(_) {
        val aa = withRep(needAtomic[SlickCreate] :: needAtomicOpt[AutoInc] :: needAtomicOpt[OneToOneCrate] :: needAtomic[FProperty] :: needAtomicOpt[DefaultValue] :: FANil)
          .mapTo {
            case (slickCreate :: autoIncOpt :: oneToOneCreateOpt :: property :: defaultOpt :: HNil, data) =>
              val isAutoInc = autoIncOpt.map(_.isAutoInc).getOrElse(false)
              val writer = if (isAutoInc) {
                lazy val oneToOneSubGen = oneToOneCreateOpt.map { oneToOneCreate =>
                  new InsertWrapTran[slickCreate.DataType] {
                    override val table = oneToOneCreate.owner
                    def convert(sourceData: slickCreate.DataType, source: InsertDataQuery): InsertDataQuery = {
                      new InsertDataQuery {
                        override val bind = source.bind
                        override val cols = source.cols ::: oneToOneCreate.mainCol :: Nil
                        override val shapes = source.shapes ::: oneToOneCreate.mainShape :: Nil
                        override val data = source.data ::: sourceData :: Nil
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
                  autalColumn = (s: slickCreate.DataType) => {
                  s
                }
                )
              } else {
                lazy val oneToOneSubGen = oneToOneCreateOpt.map { oneToOneCreate =>
                  new InsertWrapTran[Unit] {
                    override val table = oneToOneCreate.owner
                    def convert(sourceData: Unit, source: InsertDataQuery): InsertDataQuery = {
                      val FSomeValue(commonData) = data
                      new InsertDataQuery {
                        override val bind = source.bind
                        override val cols = source.cols ::: oneToOneCreate.mainCol :: Nil
                        override val shapes = source.shapes ::: oneToOneCreate.mainShape :: Nil
                        override val data = source.data ::: commonData /*oneToOneCreate.convert(commonData)*/ :: Nil
                        override val returningCols = source.returningCols
                        override val returningShapes = source.returningShapes
                        override def dataGen(returningData: List[Any]): List[DataWithIndex] = source.dataGen(returningData)
                      }
                    }
                  }
                }

                ISWriter(
                  preData = {
                  try {
                    data match {
                      case FSomeValue(data1) => data1
                      case _ => defaultOpt.map(_.value).get
                    }
                  } catch {
                    case e: MatchError =>
                      println(property.proName)
                      println(data.atomics)
                      throw e
                  }
                },
                  table = slickCreate.owner,
                  preRep = slickCreate.mainCol,
                  preShape = slickCreate.mainShape,
                  autoIncRep = (),
                  autoIncShape = implicitly[Shape[FlatShapeLevel, Unit, Unit, Unit]],
                  subGen = oneToOneSubGen,
                  autalColumn = { (s: Unit) =>
                  data match {
                    case FSomeValue(data1) => data1
                    case _ => defaultOpt.map(_.value).get
                  }
                }
                )
              }
              writer: ISlickWriter
          }
      }.aa
    } { (genList, atomicValueGen) =>
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
        CreateOperation.parseInsert(binds, genListWithIndex).map { s =>
          ExecInfo3(s.effectRows, atomicValueGen.toContent(s.columns.sortBy(_.index).map(_.data)))
        }
      }
    }.withSyntax(new PileSyntaxFunctor[CreateType[DataPileContent], CreateType] {
      override def pileMap[U](a: CreateType[DataPileContent], pervious: DataPileContent => U): CreateType[U] = {
        { binds: List[(Any, SlickQueryBindImpl)] =>
          a(binds).map { execInfo =>
            ExecInfo3(execInfo.effectRows, pervious(execInfo.columns))
          }
        }
      }
    }).withFunctor(functor)
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
    converts: List[InsWrapTran2]
  )(
    implicit
    slickProfile: JdbcProfile,
    ec: ExecutionContext
  ): slickProfile.api.DBIO[ExecInfo3[List[DataWithIndex]]] = {
    val profile = slickProfile
    import profile.api._
    try {

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

          def fillSubGenAction(autoIncData: Seq[Any]) = {
            eachWrap.zip(autoIncData).map {
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
          }

          for {
            autoIncData <- incDataDBIO
            fillSubGens = fillSubGenAction(autoIncData)
            subResult <- {
              implicit val _ = profile
              parseInsertGen(binds, insertList, fillSubGens.flatten)
            }
          } yield {
            ExecInfo3[List[DataWithIndex]](subResult.effectRows + 1, convertRetrieveQuery.dataGen(autoIncData.toList) ::: subResult.columns)
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
    insertList: List[ISlickWriterWithData]
  )(
    implicit
    slickProfile: JdbcProfile,
    ec: ExecutionContext
  ): slickProfile.api.DBIO[ExecInfo3[List[DataWithIndex]]] = {
    val profile = slickProfile
    import profile.api._
    try {
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

          def fillSubGenAction(autoIncData: Seq[Any]) = {
            eachWrap.zip(autoIncData).map {
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
          }

          for {
            autoIncData <- incDataDBIO
            fillSubGens = fillSubGenAction(autoIncData)
            subResult <- {
              implicit val _ = profile
              parseInsertGen(binds, insertList, fillSubGens.flatten)
            }
          } yield {
            ExecInfo3[List[DataWithIndex]](subResult.effectRows + 1, convertRetrieveQuery.dataGen(autoIncData.toList) ::: subResult.columns)
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