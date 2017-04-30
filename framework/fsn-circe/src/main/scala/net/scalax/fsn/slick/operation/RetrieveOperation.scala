package net.scalax.fsn.slick.operation

import net.scalax.fsn.core._
import net.scalax.fsn.slick.atomic.{ OneToOneRetrieve, SlickRetrieve }
import net.scalax.fsn.slick.helpers.{ FilterColumnGen, ListAnyShape, SlickQueryBindImpl }
import slick.dbio.DBIO
import slick.lifted._
import shapeless._
import slick.jdbc.JdbcActionComponent

import scala.concurrent.ExecutionContext

trait IWrapTran2[U] {

  val table: Any
  def convert(data: U, source: RetrieveQuery): RetrieveQuery

}
/*trait ISlickReader2 {

  type MainSColumn
  type MainDColumn
  type MainTColumn

  val mainCol: MainSColumn

  val mainShape: Shape[_ <: FlatShapeLevel, MainSColumn, MainDColumn, MainTColumn]

  val table: Any

  val autalColumn: MainDColumn => FColumn

  val primaryGen: Option[FilterColumnGen[MainTColumn]]

  val subGen: Option[IWrapTran2[MainDColumn]]

}

case class ISReader2[S, D, T](
  override val mainCol: S,
  override val table: Any,
  override val mainShape: Shape[_ <: FlatShapeLevel, S, D, T],
  override val autalColumn: D => FColumn,
  override val primaryGen: Option[FilterColumnGen[T]],
  override val subGen: Option[IWrapTran2[D]]
) extends ISlickReader2 {

  type MainSColumn = S
  type MainDColumn = D
  type MainTColumn = T

}*/
trait ISlickReader2222 {

  type MainSColumn
  type MainDColumn
  type MainTColumn

  val mainCol: MainSColumn

  val mainShape: Shape[_ <: FlatShapeLevel, MainSColumn, MainDColumn, MainTColumn]

  val table: Any

  val autalColumn: MainDColumn => Any

  val primaryGen: Option[FilterColumnGen[MainTColumn]]

  val subGen: Option[IWrapTran2[MainDColumn]]

}

case class ISReader2222[S, D, T](
    override val mainCol: S,
    override val table: Any,
    override val mainShape: Shape[_ <: FlatShapeLevel, S, D, T],
    override val autalColumn: D => Any,
    override val primaryGen: Option[FilterColumnGen[T]],
    override val subGen: Option[IWrapTran2[D]]
) extends ISlickReader2222 {

  type MainSColumn = S
  type MainDColumn = D
  type MainTColumn = T

}

trait ISlickReaderWithData {
  val reader: ISlickReader2222
  val dataGen: reader.MainDColumn => DataWithIndex
}

object InRetrieveConvert2222 {
  def convert(
    implicit
    ec: ExecutionContext,
    jsonEv: Query[_, Seq[Any], Seq] => JdbcActionComponent#StreamingQueryActionExtensionMethods[Seq[Seq[Any]], Seq[Any]]
  ) = {
    /*val slickReader = FColumn.find(columns)({ case s: SlickRetrieve[columns.DataType] => s })
    val oneToOneRetrieveOpt = FColumn.findOpt(columns)({ case s: OneToOneRetrieve[columns.DataType] => s })*/

    FPile.transformTreeList {
      new FAtomicQuery(_) {
        val aa = withRep(needAtomic[SlickRetrieve] :: needAtomicOpt[OneToOneRetrieve] :: HNil)
          .mapToOption {
            case (slickReader :: oneToOneRetrieveOpt :: HNil, data) => {
              val iSlickReader = ISReader2222(
                mainCol = (slickReader.mainCol: slickReader.SourceType),
                table = slickReader.owner,
                mainShape = slickReader.mainShape,
                autalColumn = { s: slickReader.SlickType => slickReader.convert(s) },
                primaryGen = slickReader.primaryGen.map(eachPri => new FilterColumnGen[slickReader.TargetType] {
                  override type BooleanTypeRep = eachPri.BooleanTypeRep
                  override val dataToCondition = (sourceCol: slickReader.TargetType) => {
                    eachPri.dataToCondition(sourceCol)(
                      slickReader.filterConvert(data.get)
                    )
                  }
                  override val wt = eachPri.wt
                }),
                subGen = oneToOneRetrieveOpt.map { oneToOneRetrieve =>
                  new IWrapTran2[slickReader.SlickType] {
                    override val table = oneToOneRetrieve.owner
                    override def convert(data: slickReader.SlickType, source: RetrieveQuery): RetrieveQuery = {
                      new RetrieveQuery {
                        override val bind = source.bind
                        override val cols = source.cols ::: oneToOneRetrieve.mainCol :: Nil
                        override val shapes = source.shapes ::: oneToOneRetrieve.mainShape :: Nil
                        override lazy val filters = source.filters ::: {
                          val index = cols.indexOf(oneToOneRetrieve.mainCol)
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
              iSlickReader: ISlickReader2222
            }
          }
      }.aa
    } { genList =>
      { binds: List[(Any, SlickQueryBindImpl)] =>
        val readersWithData = genList.zipWithIndex.map {
          case (reader1, index) =>
            new ISlickReaderWithData {
              override val reader: reader1.type = reader1
              override val dataGen = { mainData: reader1.MainDColumn =>
                DataWithIndex(reader1.autalColumn(mainData), index)
              }
            }
        }
        RetrieveOperation2222.parseInsertWithIndex(binds, readersWithData)
      }
    }
  }
}

trait RetrieveQuery {

  val bind: SlickQueryBindImpl
  val cols: List[Any]
  val shapes: List[Shape[_ <: FlatShapeLevel, _, _, _]]
  val filters: List[FilterColumnGen[Seq[Any]]]

}

object RetrieveOperation2222 {

  trait WrapTran2 {
    val table: Any
    def convert(source: RetrieveQuery): RetrieveQuery
  }

  def parseInsertGen(
    binds: List[(Any, SlickQueryBindImpl)],
    retrieveList: List[ISlickReaderWithData],
    converts: List[WrapTran2]
  )(
    implicit
    ec: ExecutionContext,
    jsonEv: Query[_, Seq[Any], Seq] => JdbcActionComponent#StreamingQueryActionExtensionMethods[Seq[Seq[Any]], Seq[Any]]
  ): DBIO[ExecInfo3] = try {
    val wrapList = retrieveList //.map(InRetrieveConvert2.convert)

    val currents = wrapList.groupBy(_.reader.table).filter { case (key, s) => converts.exists(t => key == t.table) }
    val results = currents.map {
      case (table, eachWrap) =>
        val initRetrieveQuery: RetrieveQuery = new RetrieveQuery {
          override val bind = binds.find(_._1 == table).get._2
          override val cols = eachWrap.map(_.reader.mainCol)
          override val shapes = eachWrap.map(_.reader.mainShape)
          override val filters = eachWrap.zipWithIndex.map {
            case (gen, index) =>
              gen.reader.primaryGen.map { priGen =>
                new FilterColumnGen[Seq[Any]] {
                  override type BooleanTypeRep = priGen.BooleanTypeRep
                  override val dataToCondition = { cols: Seq[Any] =>
                    priGen.dataToCondition(cols(index).asInstanceOf[gen.reader.MainTColumn])
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
          (fillCols, fillSubGens) = eachWrap.zip(retrieveData).map {
            case (wrap, dataItem) =>
              val wrapSlickData = dataItem.asInstanceOf[wrap.reader.MainDColumn]
              val newCols = wrap.dataGen(wrapSlickData)
              val subGens = wrap.reader.subGen.map { gen =>
                new WrapTran2 {
                  override val table = gen.table
                  override def convert(source: RetrieveQuery): RetrieveQuery = {
                    gen.convert(wrapSlickData, source)
                  }
                }
              }
              (newCols -> subGens)
          }.unzip
          subResult <- parseInsertGen(binds, retrieveList, fillSubGens.map(_.toList).flatten)
        } yield {
          ExecInfo3(subResult.effectRows + 1, subResult.columns ::: fillCols)
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

  def parseInsertWithIndex(
    binds: List[(Any, SlickQueryBindImpl)],
    retrieveList: List[ISlickReaderWithData]
  )(
    implicit
    ec: ExecutionContext,
    jsonEv: Query[_, Seq[Any], Seq] => JdbcActionComponent#StreamingQueryActionExtensionMethods[Seq[Seq[Any]], Seq[Any]]
  ): DBIO[ExecInfo3] = try {
    val wrapList = retrieveList //.map(InRetrieveConvert2.convert)

    val subGensTables = wrapList.flatMap { t => t.reader.subGen.toList.map(_.table) }
    val currents = wrapList.groupBy(_.reader.table).filter { case (key, s) => subGensTables.forall(t => key != t) }
    val results = currents.map {
      case (table, eachWrap) =>
        val initRetrieveQuery: RetrieveQuery = new RetrieveQuery {
          override val bind = binds.find(_._1 == table).get._2
          override val cols = eachWrap.map(_.reader.mainCol)
          override val shapes = eachWrap.map(_.reader.mainShape)
          override val filters = eachWrap.zipWithIndex.map {
            case (gen, index) =>
              gen.reader.primaryGen.map { priGen =>
                new FilterColumnGen[Seq[Any]] {
                  override type BooleanTypeRep = priGen.BooleanTypeRep
                  override val dataToCondition = { cols: Seq[Any] =>
                    priGen.dataToCondition(cols(index).asInstanceOf[gen.reader.MainTColumn])
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
          (fillCols, fillSubGens) = eachWrap.zip(retrieveData).map {
            case (wrap, dataItem) =>
              val wrapSlickData = dataItem.asInstanceOf[wrap.reader.MainDColumn]
              val newCols = wrap.dataGen(wrapSlickData)
              val subGens = wrap.reader.subGen.map { gen =>
                new WrapTran2 {
                  override val table = gen.table
                  override def convert(source: RetrieveQuery): RetrieveQuery = {
                    gen.convert(wrapSlickData, source)
                  }
                }
              }
              (newCols -> subGens)
          }.unzip
          subResult <- parseInsertGen(binds, retrieveList, fillSubGens.map(_.toList).flatten)
        } yield {
          ExecInfo3(subResult.effectRows + 1, subResult.columns ::: fillCols) //UpdateStaticManyInfo(subResult.effectRows + 1, subResult.many ++ Map())
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
  /*def parseInsert(
                   binds: List[(Any, SlickQueryBindImpl)],
                   retrieveList: List[ISlickReaderWithData]
                 )(
                   implicit
                   ec: ExecutionContext,
                   jsonEv: Query[_, Seq[Any], Seq] => BasicProfile#StreamingQueryActionExtensionMethods[Seq[Seq[Any]], Seq[Any]]
                 ): DBIO[ExecInfo3] = {
    parseInsertWithIndex(binds, retrieveList)
  }*/
}
/*case class ExecInfo2(effectRows: Int, columns: List[FColumn])

object RetrieveOperation {

  trait WrapTran2 {
    val table: Any
    def convert(source: RetrieveQuery): RetrieveQuery
  }

  def parseInsertGen(
    binds: List[(Any, SlickQueryBindImpl)],
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
    binds: List[(Any, SlickQueryBindImpl)],
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
    binds: List[(Any, SlickQueryBindImpl)],
    retrieveList: List[FColumn]
  )(
    implicit
    ec: ExecutionContext,
    jsonEv: Query[_, Seq[Any], Seq] => BasicProfile#StreamingQueryActionExtensionMethods[Seq[Seq[Any]], Seq[Any]]
  ): DBIO[ExecInfo2] = {
    parseInsertWithIndex(binds, retrieveList)
  }

}*/ 