package net.scalax.fsn.slick.operation

import net.scalax.fsn.core.{FColumn, FsnColumn}
import net.scalax.fsn.slick.atomic.{OneToOneRetrieve, SlickRetrieve}
import net.scalax.fsn.slick.helpers.{FilterColumnGen, ListAnyShape, SlickQueryBindImpl}
import slick.basic.BasicProfile
import slick.dbio.DBIO
import slick.lifted._
import slick.relational.RelationalProfile

import scala.concurrent.ExecutionContext
import scala.language.existentials

trait IWrapTran2[U] {

  val table: RelationalProfile#Table[_]
  def convert(data: U, source: RetrieveQuery): RetrieveQuery

}

trait ISlickReader2 {

  type MainSColumn
  type MainDColumn
  type MainTColumn

  val mainCol: MainSColumn

  val mainShape: Shape[_ <: FlatShapeLevel, MainSColumn, MainDColumn, MainTColumn]

  val table: RelationalProfile#Table[_]

  val autalColumn: MainDColumn => FColumn

  val primaryGen: Option[FilterColumnGen[MainTColumn]]

  val subGen: Option[IWrapTran2[MainDColumn]]

}

case class ISReader2[S, D, T](
  override val mainCol: S,
  override val table: RelationalProfile#Table[_],
  override val mainShape: Shape[_ <: FlatShapeLevel, S, D, T],
  override val autalColumn: D => FColumn,
  override val primaryGen: Option[FilterColumnGen[T]],
  override val subGen: Option[IWrapTran2[D]]
) extends ISlickReader2 {

  type MainSColumn = S
  type MainDColumn = D
  type MainTColumn = T

}

object InRetrieveConvert2 {
  def convert(columns: FColumn)(implicit ec: ExecutionContext): ISlickReader2 = {
    val slickReader = FColumn.find(columns)({ case s: SlickRetrieve[columns.DataType] => s })
    val oneToOneRetrieveOpt = FColumn.findOpt(columns)({ case s: OneToOneRetrieve[columns.DataType] => s })

    val iSlickReader = ISReader2(
      mainCol = (slickReader.mainCol.rep: slickReader.SourceType),
      table = slickReader.mainCol.owner,
      mainShape = slickReader.mainShape,
      autalColumn = { s: slickReader.SlickType => FsnColumn(columns.cols, Option(slickReader.convert(s))) },
      primaryGen = slickReader.primaryGen.map(eachPri => new FilterColumnGen[slickReader.TargetType] {
        override type BooleanTypeRep = eachPri.BooleanTypeRep
        override val dataToCondition = (sourceCol: slickReader.TargetType) => {
          eachPri.dataToCondition(sourceCol)(
            slickReader.filterConvert(columns.data.get)
          )
        }
        override val wt = eachPri.wt
      }),
      subGen = oneToOneRetrieveOpt.map { oneToOneRetrieve =>
        new IWrapTran2[slickReader.SlickType] {
          override val table = oneToOneRetrieve.mainCol.owner
          override def convert(data: slickReader.SlickType, source: RetrieveQuery): RetrieveQuery = {
            new RetrieveQuery {
              override val bind = source.bind
              override val cols = source.cols ::: oneToOneRetrieve.mainCol.rep :: Nil
              override val shapes = source.shapes ::: oneToOneRetrieve.mainShape :: Nil
              override lazy val filters = source.filters ::: {
                val index = cols.indexOf(oneToOneRetrieve.mainCol.rep)
                new FilterColumnGen[Seq[Any]] {
                  override type BooleanTypeRep = oneToOneRetrieve.primaryGen.BooleanTypeRep
                  override val dataToCondition = { cols: Seq[Any] =>
                    val col = cols(index).asInstanceOf[oneToOneRetrieve.TargetType]
                    val slickData = oneToOneRetrieve.filterConvert(slickReader.convert(data))
                    oneToOneRetrieve.primaryGen.dataToCondition(col)(slickData)
                  }
                  override val wt = oneToOneRetrieve.primaryGen.wt
                }
              } :: Nil
            }
          }
        }

      }
    )
    iSlickReader
  }
}

trait RetrieveQuery {

  val bind: SlickQueryBindImpl
  val cols: List[Any]
  val shapes: List[Shape[_ <: FlatShapeLevel, _, _, _]]
  val filters: List[FilterColumnGen[Seq[Any]]]

}

case class ExecInfo2(effectRows: Int, columns: List[FColumn])

object RetrieveOperation {

  trait WrapTran2 {
    val table: RelationalProfile#Table[_]
    def convert(source: RetrieveQuery): RetrieveQuery
  }

  def parseInsertGen(
    binds: List[(RelationalProfile#Table[_], SlickQueryBindImpl)],
    retrieveList: List[FColumn],
    converts: List[WrapTran2]
  )(
    implicit
    ec: ExecutionContext,
    jsonEv: Query[_, Seq[Any], Seq] => BasicProfile#StreamingQueryActionExtensionMethods[Seq[Seq[Any]], Seq[Any]]
  ): DBIO[ExecInfo2] = try {
    val wrapList = retrieveList.map(InRetrieveConvert2.convert)

    val currents = wrapList.groupBy(_.table).filter { case (key, s) => converts.exists(t => key == t.table) }
    val results = currents.map { case (table, eachWrap) =>
      val initRetrieveQuery: RetrieveQuery = new RetrieveQuery {
        override val bind = binds.find(_._1 == table).get._2
        override val cols = eachWrap.map(_.mainCol)
        override val shapes = eachWrap.map(_.mainShape)
        override val filters = eachWrap.zipWithIndex.map { case (gen, index) =>
          gen.primaryGen.map { priGen =>
            new FilterColumnGen[Seq[Any]] {
              override type BooleanTypeRep = priGen.BooleanTypeRep
              override val dataToCondition = { cols: Seq[Any] =>
                priGen.dataToCondition(cols(index).asInstanceOf[gen.MainTColumn])
              }
              override val wt = priGen.wt
            }
          }.toList: List[FilterColumnGen[Seq[Any]]]
        }.flatten
      }
      val convertRetrieveQuery = converts.filter(_.table == table).foldLeft(initRetrieveQuery) { (x, y) =>
        y.convert(x)
      }
      val query = Query(convertRetrieveQuery.cols)(new ListAnyShape[FlatShapeLevel](convertRetrieveQuery.shapes))
      val bindQuery = convertRetrieveQuery.bind.bind(query)
      val filterQuery = convertRetrieveQuery.filters.foldLeft(bindQuery) { (x, y) =>
        x.filter(s => y.dataToCondition(s))(y.wt)
      }
      for {
        retrieveData <- filterQuery.result.head
        (fillCols, fillSubGens) = eachWrap.zip(retrieveData).map { case (wrap, dataItem) =>
          val wrapSlickData = dataItem.asInstanceOf[wrap.MainDColumn]
          val newCols = wrap.autalColumn(wrapSlickData)
          val subGens = wrap.subGen.map { gen => new WrapTran2 {
            override val table = gen.table
            override def convert(source: RetrieveQuery): RetrieveQuery = {
              gen.convert(wrapSlickData, source)
            }
          } }
          (newCols -> subGens)
        }.unzip
        subResult <- parseInsertGen(binds, retrieveList, fillSubGens.map(_.toList).flatten)
      } yield {
        ExecInfo2(subResult.effectRows + 1, subResult.columns ::: fillCols)
      }

    }

    results.foldLeft(DBIO.successful(ExecInfo2(0, Nil)): DBIO[ExecInfo2]) { (s, t) =>
      (for {
        s1 <- s
        t1 <- t
      } yield {
        ExecInfo2(s1.effectRows + t1.effectRows, s1.columns ::: t1.columns)
      })
    }
  } catch {
    case e: Exception =>
      DBIO.failed(e)
  }

  def parseInsertWithIndex(
    binds: List[(RelationalProfile#Table[_], SlickQueryBindImpl)],
    retrieveList: List[FColumn]
  )(
    implicit
    ec: ExecutionContext,
    jsonEv: Query[_, Seq[Any], Seq] => BasicProfile#StreamingQueryActionExtensionMethods[Seq[Seq[Any]], Seq[Any]]
  ): DBIO[ExecInfo2] = try {
    val wrapList = retrieveList.map(InRetrieveConvert2.convert)

    val subGensTables = wrapList.flatMap { t => t.subGen.toList.map(_.table) }
    val currents = wrapList.groupBy(_.table).filter { case (key, s) => subGensTables.forall(t => key != t) }
    val results = currents.map { case (table, eachWrap) =>
      val initRetrieveQuery: RetrieveQuery = new RetrieveQuery {
        override val bind = binds.find(_._1 == table).get._2
        override val cols = eachWrap.map(_.mainCol)
        override val shapes = eachWrap.map(_.mainShape)
        override val filters = eachWrap.zipWithIndex.map { case (gen, index) =>
          gen.primaryGen.map { priGen =>
            new FilterColumnGen[Seq[Any]] {
              override type BooleanTypeRep = priGen.BooleanTypeRep
              override val dataToCondition = { cols: Seq[Any] =>
                priGen.dataToCondition(cols(index).asInstanceOf[gen.MainTColumn])
              }
              override val wt = priGen.wt
            }
          }.toList: List[FilterColumnGen[Seq[Any]]]
        }.flatten
      }
      val convertRetrieveQuery = initRetrieveQuery
      val query = Query(convertRetrieveQuery.cols)(new ListAnyShape[FlatShapeLevel](convertRetrieveQuery.shapes))
      val bindQuery = convertRetrieveQuery.bind.bind(query)
      val filterQuery = convertRetrieveQuery.filters.foldLeft(bindQuery) { (x, y) =>
        x.filter(s => y.dataToCondition(s))(y.wt)
      }
      for {
        retrieveData <- filterQuery.result.head
        (fillCols, fillSubGens) = eachWrap.zip(retrieveData).map { case (wrap, dataItem) =>
          val wrapSlickData = dataItem.asInstanceOf[wrap.MainDColumn]
          val newCols = wrap.autalColumn(wrapSlickData)
          val subGens = wrap.subGen.map { gen => new WrapTran2 {
            override val table = gen.table
            override def convert(source: RetrieveQuery): RetrieveQuery = {
              gen.convert(wrapSlickData, source)
            }
          } }
          (newCols -> subGens)
        }.unzip
        subResult <- parseInsertGen(binds, retrieveList, fillSubGens.map(_.toList).flatten)
      } yield {
        ExecInfo2(subResult.effectRows + 1, subResult.columns ::: fillCols)//UpdateStaticManyInfo(subResult.effectRows + 1, subResult.many ++ Map())
      }

    }

    results.foldLeft(DBIO.successful(ExecInfo2(0, Nil)): DBIO[ExecInfo2]) { (s, t) =>
      (for {
        s1 <- s
        t1 <- t
      } yield {
        ExecInfo2(s1.effectRows + t1.effectRows, s1.columns ::: t1.columns)
      })
    }
  } catch {
    case e: Exception =>
      DBIO.failed(e)
  }

  def parseInsert(
    binds: List[(RelationalProfile#Table[_], SlickQueryBindImpl)],
    retrieveList: List[FColumn]
  )(
    implicit
    ec: ExecutionContext,
    jsonEv: Query[_, Seq[Any], Seq] => BasicProfile#StreamingQueryActionExtensionMethods[Seq[Seq[Any]], Seq[Any]]
  ): DBIO[ExecInfo2] = {
    parseInsertWithIndex(binds, retrieveList)
  }

}