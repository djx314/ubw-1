package net.scalax.fsn.slick.operation

import net.scalax.fsn.common.atomic.FProperty
import net.scalax.fsn.core._
import net.scalax.fsn.json.operation.{ AtomicValueHelper, FSomeValue }
import net.scalax.fsn.slick.atomic.{ AutoInc, OneToOneCrate, SlickCreate }
import net.scalax.fsn.slick.helpers.{ ListAnyShape, SlickQueryBindImpl, SlickUtils }
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

  def createGen(
    implicit
    slickProfile: JdbcProfile,
    ec: ExecutionContext
  ): PileSyntax.PileGen[List[(Any, SlickQueryBindImpl)] => slickProfile.api.DBIO[ExecInfo3]] = {
    Pile.transformTreeList {
      new AtomicQuery(_) {
        import slickProfile.api._

        val aa = withRep(needAtomic[SlickCreate] :: needAtomicOpt[AutoInc] :: needAtomicOpt[OneToOneCrate] :: needAtomic[FProperty] :: FANil)
          .mapTo {
            case (slickCreate :: autoIncOpt :: oneToOneCreateOpt :: property :: HNil, data) =>
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
                  //slickCreate.convert(s)
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
                    val FSomeValue(data1) = data
                    //slickCreate.reverseConvert(data1)
                    data1
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
                  val FSomeValue(data1) = data
                  data1
                }
                )
              }
              writer: ISlickWriter
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
    converts: List[InsWrapTran2]
  )(
    implicit
    slickProfile: JdbcProfile,
    ec: ExecutionContext
  ): slickProfile.api.DBIO[ExecInfo3] = try {
    val slickProfileI = slickProfile
    import slickProfile.api._

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
          (queryInsertActionExtensionMethods(bindQuery) += convertRetrieveQuery.data) >> streamableQueryActionExtensionMethods(returingQuery).result.head
        } else {
          val bindReturingQuery = convertRetrieveQuery.bind.bind(returingQuery)
          val createQuery = queryInsertActionExtensionMethods(bindQuery) returning bindReturingQuery
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
            implicit val _ = slickProfileI
            parseInsertGen(binds, insertList, fillSubGens.flatten)
          }
        } yield {
          ExecInfo3(subResult.effectRows + 1, convertRetrieveQuery.dataGen(autoIncData.toList) ::: subResult.columns)
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
      slickProfile.api.DBIO.failed(e)
  }

  def parseInsert(
    binds: List[(Any, SlickQueryBindImpl)],
    insertList: List[ISlickWriterWithData]
  )(
    implicit
    slickProfile: JdbcProfile,
    ec: ExecutionContext
  //cv: Query[_, Seq[Any], Seq] => JdbcActionComponent#InsertActionExtensionMethods[Seq[Any]],
  //retrieveCv: Query[_, Seq[Any], Seq] => JdbcActionComponent#StreamingQueryActionExtensionMethods[Seq[Seq[Any]], Seq[Any]]
  ): slickProfile.api.DBIO[ExecInfo3] = try {
    val slickProfileI = slickProfile
    import slickProfile.api._

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
          (queryInsertActionExtensionMethods(bindQuery) += convertRetrieveQuery.data) >> streamableQueryActionExtensionMethods(returingQuery).result.head
        } else {
          val bindReturingQuery = convertRetrieveQuery.bind.bind(returingQuery)
          val createQuery = queryInsertActionExtensionMethods(bindQuery) returning bindReturingQuery
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
            implicit val _ = slickProfileI
            parseInsertGen(binds, insertList, fillSubGens.flatten)
          }
        } yield {
          ExecInfo3(subResult.effectRows + 1, convertRetrieveQuery.dataGen(autoIncData.toList) ::: subResult.columns)
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
      slickProfile.api.DBIO.failed(e)
  }

}