/*package indicator.rw.utils.rw2

import aaaa.FilterColumnGen
import indicator.rw.utils.SlickQueryBindImpl
import indicator.rw.utils.rw.{OneToOneRetrieve, SlickRetrieve}
import net.scalax.fsn.core.{FColumn, FsnColumn}
import slick.basic.BasicProfile
import slick.dbio.DBIO
import slick.lifted._
import slick.relational.RelationalProfile

import scala.concurrent.ExecutionContext
import scala.language.existentials
import scalaz.Semigroup

trait IWrapTran[U] {

  val table: RelationalProfile#Table[_]
  def convert(data: U, source: ISlickReader): ISlickReader

}

trait ISlickReader {

  type MainSColumn
  type MainDColumn
  type MainTColumn

  val mainCol: MainSColumn

  val mainShape: Shape[_ <: FlatShapeLevel, MainSColumn, MainDColumn, MainTColumn]

  val table: RelationalProfile#Table[_]

  val autalColumn: MainDColumn => List[ColumnWithIndex]

  val primaryGen: List[FilterColumnGen[MainTColumn]]

  val subGen: List[IWrapTran[MainDColumn]]

}

case class ISReader[S, D, T](
  override val mainCol: S,
  override val table: RelationalProfile#Table[_],
  override val mainShape: Shape[_ <: FlatShapeLevel, S, D, T],
  override val autalColumn: D => List[ColumnWithIndex],
  override val primaryGen: List[FilterColumnGen[T]],
  override val subGen: List[IWrapTran[D]]
) extends ISlickReader {

  type MainSColumn = S
  type MainDColumn = D
  type MainTColumn = T

}

object InRetrieveConvert {
  def convert(columns: ColumnWithIndex)(implicit ec: ExecutionContext): ISlickReader = {
    val slickReader = FColumn.find(columns.column)({ case s: SlickRetrieve[columns.column.DataType] => s })
    val oneToOneRetrieveOpt = FColumn.findOpt(columns.column)({ case s: OneToOneRetrieve[columns.column.DataType] => s })

    val iSlickReader = ISReader(
      mainCol = (slickReader.mainCol.rep: slickReader.SourceType),
      table = slickReader.mainCol.owner,
      mainShape = slickReader.mainShape,
      autalColumn = (s: slickReader.SlickType) => List(ColumnWithIndex(
        FsnColumn(columns.column.cols/*, columns.column.proName*/, Option(slickReader.convert(s))),
        columns.index
      )),
      primaryGen = slickReader.primaryGen.toList.map(eachPri => new FilterColumnGen[slickReader.TargetType] {
        override type BooleanTypeRep = eachPri.BooleanTypeRep
        override val dataToCondition = (sourceCol: slickReader.TargetType) => {
          eachPri.dataToCondition(sourceCol)(
            slickReader.filterConvert(columns.column.data.get)
          )
        }
        override val wt = eachPri.wt
      }),
      subGen = oneToOneRetrieveOpt.map { oneToOneRetrieve =>
        val newTran = new IWrapTran[slickReader.SlickType] {
          override val table = oneToOneRetrieve.mainCol.owner
          override def convert(data: slickReader.SlickType, source: ISlickReader): ISlickReader = {
            val newSource = ISReader(
              mainCol = source.mainCol -> (oneToOneRetrieve.mainCol.rep: oneToOneRetrieve.SourceType),
              table = oneToOneRetrieve.mainCol.owner,
              mainShape = Shape.tuple2Shape(source.mainShape, oneToOneRetrieve.mainShape),
              autalColumn = { s: (source.MainDColumn, oneToOneRetrieve.SlickType) => source.autalColumn(s._1) },
              primaryGen = {
                val sourcePriGen = source.primaryGen.map { eachPrimaryGen =>
                  new FilterColumnGen[(source.MainTColumn, oneToOneRetrieve.TargetType)] {
                    override type BooleanTypeRep = eachPrimaryGen.BooleanTypeRep
                    override val dataToCondition = { filterCol: (source.MainTColumn, oneToOneRetrieve.TargetType) =>
                      eachPrimaryGen.dataToCondition(filterCol._1)
                    }
                    override val wt = eachPrimaryGen.wt
                  }
                }
                val filterOneToOneGen = {
                  new FilterColumnGen[(source.MainTColumn, oneToOneRetrieve.TargetType)] {
                    override type BooleanTypeRep = oneToOneRetrieve.primaryGen.BooleanTypeRep
                    override val dataToCondition = { filterCol: (source.MainTColumn, oneToOneRetrieve.TargetType) =>
                      oneToOneRetrieve.primaryGen.dataToCondition(filterCol._2)(
                        oneToOneRetrieve.filterConvert(slickReader.convert(data))
                      )
                    }
                    override val wt = oneToOneRetrieve.primaryGen.wt
                  }
                }
                filterOneToOneGen :: sourcePriGen
              },
              subGen =
                source.subGen.map { eachGen =>
                  new IWrapTran[(source.MainDColumn, oneToOneRetrieve.SlickType)] {
                    override val table = eachGen.table
                    override def convert(data: (source.MainDColumn, oneToOneRetrieve.SlickType), source1: ISlickReader): ISlickReader = {
                      eachGen.convert(data._1, source1)
                    }
                  }
                }
            )
            newSource
          }
        }
        newTran
      }.toList
    )
    iSlickReader
  }
}

trait IlickMonad {

  implicit def dJsonSlick(implicit ec: ExecutionContext): Semigroup[ISlickReader] = new Semigroup[ISlickReader] {
    def append(f1: ISlickReader, f2: => ISlickReader): ISlickReader = {
      val f2Case = f2
      ISReader(
        mainCol = f1.mainCol -> f2Case.mainCol,
        mainShape = Shape.tuple2Shape(f1.mainShape, f2Case.mainShape),
        table = (f1.table :: f2Case.table :: Nil).distinct match {
          case headTable :: Nil =>
            headTable
          case _ =>
            throw new Exception("要合并的 2 个 reader 来自不同的表11111111")
        },
        autalColumn = { s: (f1.MainDColumn, f2Case.MainDColumn) =>
          f1.autalColumn(s._1) ::: f2Case.autalColumn(s._2)
        },
        primaryGen = {
          val f1List = f1.primaryGen.map { eachPrimary =>
            new FilterColumnGen[(f1.MainTColumn, f2Case.MainTColumn)] {

              override type BooleanTypeRep = eachPrimary.BooleanTypeRep

              override val dataToCondition = { s: (f1.MainTColumn, f2Case.MainTColumn) =>
                eachPrimary.dataToCondition(s._1)
              }
              override val wt = eachPrimary.wt

            }
          }
          val f2List = f2Case.primaryGen.map { eachPrimary =>
            new FilterColumnGen[(f1.MainTColumn, f2Case.MainTColumn)] {

              override type BooleanTypeRep = eachPrimary.BooleanTypeRep

              override val dataToCondition = { s: (f1.MainTColumn, f2Case.MainTColumn) =>
                eachPrimary.dataToCondition(s._2)
              }
              override val wt = eachPrimary.wt

            }
          }
          f1List ::: f2List
        },
        subGen = f1.subGen.map { eachGen =>
          new IWrapTran[(f1.MainDColumn, f2Case.MainDColumn)] {

            override val table = eachGen.table
            override def convert(data: (f1.MainDColumn, f2Case.MainDColumn), source: ISlickReader): ISlickReader = {
              eachGen.convert(data._1, source)
            }

          }
        } ::: f2Case.subGen.map { eachGen =>
          new IWrapTran[(f1.MainDColumn, f2Case.MainDColumn)] {

            override val table = eachGen.table
            override def convert(data: (f1.MainDColumn, f2Case.MainDColumn), source: ISlickReader): ISlickReader = {
              eachGen.convert(data._2, source)
            }

          }
        }
      )
    }
  }

}

object RetrieveWrapDeal extends IlickMonad {

  trait WrapTran {
    val table: RelationalProfile#Table[_]
    def convert(source: ISlickReader): ISlickReader
  }

  def parseInsertGen(
    binds: List[(RelationalProfile#Table[_], SlickQueryBindImpl)],
    insertListWrap: List[ColumnWithIndex],
    converts: List[WrapTran]
  )(
    implicit
    ec: ExecutionContext,
    retrieve: Query[_, String, Seq] => BasicProfile#StreamingQueryActionExtensionMethods[Seq[String], String]
  ): DBIO[ExecInfo] = try {
    val zipDeal = implicitly[scalaz.Semigroup[ISlickReader]]

    val wrapList = insertListWrap.map(InRetrieveConvert.convert)

    val currents = wrapList.groupBy(_.table).filter { case (key, s) => converts.exists(t => key == t.table) }.map { case (key, writers) =>
      writers.reduce { (s, t) => zipDeal.append(s, t) }
    }
    val results = currents.map { eachWrap =>
      val autualWrap = converts.filter(s => s.table == eachWrap.table).foldLeft(eachWrap) { (x, y) =>
        y.convert(x)
      }
      val bind = binds.find(s => s._1 == autualWrap.table).map(_._2).get
      val retrieveQuery = bind.bind(Query(autualWrap.mainCol)(autualWrap.mainShape))
      val filterQuery = autualWrap.primaryGen.foldLeft(retrieveQuery) { (query, gen) =>
        query.filter(filterCol => gen.dataToCondition(filterCol))(gen.wt)
      }
      val insertDBIO: DBIO[autualWrap.MainDColumn] = {
        val convert = retrieve.asInstanceOf[Query[_, autualWrap.MainDColumn, Seq] => BasicProfile#StreamingQueryActionExtensionMethods[Seq[autualWrap.MainDColumn], autualWrap.MainDColumn]]
        convert(filterQuery).result.head
      }
      for {
        incData <- insertDBIO
        listToMerge = autualWrap.autalColumn(incData)
        subs = autualWrap.subGen.map { u =>
          new WrapTran {
            override val table = u.table
            override def convert(source: ISlickReader): ISlickReader = {
              u.convert(incData, source)
            }
          }
        }
        subResult <- parseInsertGen(binds, insertListWrap, subs)
      } yield {
        ExecInfo(subResult.effectRows + 1, subResult.columns ::: listToMerge)//UpdateStaticManyInfo(subResult.effectRows + 1, subResult.many ++ Map())
      }
    }
    results.foldLeft(DBIO.successful(ExecInfo(0, Nil)): DBIO[ExecInfo]) { (s, t) =>
      (for {
        s1 <- s
        t1 <- t
      } yield {
        ExecInfo(s1.effectRows + t1.effectRows, s1.columns ::: t1.columns)
      })
    }
  } catch {
    case e: Exception =>
      DBIO.failed(e)
  }

  def parseInsertWithIndex(
    binds: List[(RelationalProfile#Table[_], SlickQueryBindImpl)],
    insertListWrap: List[ColumnWithIndex]
  )(
    implicit
    ec: ExecutionContext,
    retrieve: Query[_, String, Seq] => BasicProfile#StreamingQueryActionExtensionMethods[Seq[String], String]
  ): DBIO[ExecInfo] = try {
    val zipDeal = implicitly[scalaz.Semigroup[ISlickReader]]

    val wrapList = insertListWrap.map(InRetrieveConvert.convert)
    val dealInserts = wrapList.groupBy(_.table).map(_._2).map(s => s.reduce { (u, v) => zipDeal.append(u, v) }).toList
    val tables = dealInserts.flatMap { t => t.subGen.map(_.table) }
    val currents = dealInserts.filter { s =>
      tables.forall(t => t != s.table)
    }
    val results = currents.map { eachWrap =>
      val autualWrap = eachWrap
      val bind = binds.find(s => s._1 == autualWrap.table).map(_._2).get
      val retrieveQuery = bind.bind(Query(autualWrap.mainCol)(autualWrap.mainShape))
      val filterQuery = autualWrap.primaryGen.foldLeft(retrieveQuery) { (query, gen) =>
        query.filter(filterCol => gen.dataToCondition(filterCol))(gen.wt)
      }
      val insertDBIO: DBIO[autualWrap.MainDColumn] = {
        val convert = retrieve.asInstanceOf[Query[_, autualWrap.MainDColumn, Seq] => BasicProfile#StreamingQueryActionExtensionMethods[Seq[autualWrap.MainDColumn], autualWrap.MainDColumn]]
        convert(filterQuery).result.headOption.map { s =>
          //println(s.toString + "2333" * 100)
          s.get
        }
      }
      for {
        incData <- insertDBIO
        listToMerge = autualWrap.autalColumn(incData)
        subs = autualWrap.subGen.map { u =>
          new WrapTran {
            override val table = u.table
            override def convert(source: ISlickReader): ISlickReader = {
              u.convert(incData, source)
            }
          }
        }
        subResult <- parseInsertGen(binds, insertListWrap, subs)
      } yield {
        ExecInfo(subResult.effectRows + 1, subResult.columns ::: listToMerge)//UpdateStaticManyInfo(subResult.effectRows + 1, subResult.many ++ Map())
      }
    }
    results.foldLeft(DBIO.successful(ExecInfo(0, Nil)): DBIO[ExecInfo]) { (s, t) =>
      (for {
        s1 <- s
        t1 <- t
      } yield {
        ExecInfo(s1.effectRows + t1.effectRows, s1.columns ::: t1.columns)
      })
    }
  } catch {
    case e: Exception =>
      DBIO.failed(e)
  }

  def parseInsert(
    binds: List[(RelationalProfile#Table[_], SlickQueryBindImpl)],
    insertListWrap: List[FColumn]
  )(
    implicit
    ec: ExecutionContext,
    retrieve: Query[_, String, Seq] => BasicProfile#StreamingQueryActionExtensionMethods[Seq[String], String]
  ): DBIO[ExecInfo] = {
    parseInsertWithIndex(binds, insertListWrap.zipWithIndex.map(ColumnWithIndex.tupled)).map { s => s.copy(columns = s.columns.sortBy(_.index)) }
  }

}*/