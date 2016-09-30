/*package indicator.rw.utils.rw2

import indicator.rw.utils.{SlickQueryBindImpl, SlickUtils}
import indicator.rw.utils.rw.{AutoInc, FProperty, OneToOneCrate, SlickCreate}
import net.scalax.fsn.core.{FColumn, FsnColumn}
import slick.basic.BasicProfile
import slick.jdbc.JdbcActionComponent
import slick.lifted.{FlatShapeLevel, Shape}
import slick.relational.RelationalProfile

import scala.concurrent.ExecutionContext
import scala.language.existentials

case class ExecInfo(effectRows: Int, columns: List[ColumnWithIndex]) {

  def fColumns = columns.map(_.column)

}

trait CWrapTran[I] {
  val table: RelationalProfile#Table[_]
  def convert(inc: I, source: ISlickWriter): ISlickWriter
}

case class ColumnWithIndex(column: FColumn, index: Int)

trait ISlickWriter {
  type PreRep
  type PreValue
  type PreTarget
  type IncRep
  type IncValue
  type IncTarget
  type ExtRep
  type ExtValue
  type ExtTarget

  val extData: ExtValue
  val preData: PreValue
  val table: RelationalProfile#Table[_]
  val preRep: PreRep
  val preShape: Shape[_ <: FlatShapeLevel, PreRep, PreValue, PreTarget]
  val autoIncRep: IncRep
  val autoIncShape: Shape[_ <: FlatShapeLevel, IncRep, IncValue, IncTarget]
  val extRep: ExtRep
  val extShape: Shape[_ <: FlatShapeLevel, ExtRep, ExtValue, ExtTarget]
  //val manyGen: Future[Map[String, QueryJsonInfo]]
  val subGen: List[CWrapTran[IncValue]]
  val autaulCol: IncValue => List[ColumnWithIndex]
}

case class ISWriter[A, B, C, D, E, F, G, H, I](
  override val extData: H,
  override val preData: B,
  override val table: RelationalProfile#Table[_],
  override val preRep: A,
  override val preShape: Shape[_ <: FlatShapeLevel, A, B, C],
  override val autoIncRep: D,
  override val autoIncShape: Shape[_ <: FlatShapeLevel, D, E, F],
  override val extRep: G,
  override val extShape: Shape[_ <: FlatShapeLevel, G, H, I],
  //fmanyGen: => Future[Map[String, QueryJsonInfo]],
  override val subGen: List[CWrapTran[E]],
  override val autaulCol: E => List[ColumnWithIndex]
) extends ISlickWriter {
  override type PreRep = A
  override type PreValue = B
  override type PreTarget = C
  override type IncRep = D
  override type IncValue = E
  override type IncTarget = F
  override type ExtRep = G
  override type ExtValue = H
  override type ExtTarget = I
}

object InCreateConvert {
  def convert(columns: ColumnWithIndex)(implicit ec: ExecutionContext): ISlickWriter = {
    val slickCreate = FColumn.find(columns.column) { case s: SlickCreate[columns.column.DataType] => s }
    val isAutoInc = FColumn.findOpt(columns.column) { case s : AutoInc[columns.column.DataType] => s }.map(_.isAutoInc).getOrElse(false)
    val oneToOneCreateOpt = FColumn.findOpt(columns.column) { case s: OneToOneCrate[columns.column.DataType] => s }
    //val staticManyList = FColumn.filter(columns) { case s: StaticMany[columns.DataType] => s }

    if (isAutoInc) {
      lazy val oneToOneSubGen = oneToOneCreateOpt.map { s => new CWrapTran[slickCreate.SlickType] {
        override val table = s.mainCol.owner

        def convert(data: slickCreate.SlickType, source: ISlickWriter): ISlickWriter = {
          val sourceCol: s.SourceType = s.mainCol.rep
          if (s.mainCol.owner != source.table) {
            throw new Exception("扩展插入列与该对象的的其他列属于不同的表")
          }
          //println("1234 " * 100)
          ISWriter(
            extData = s.convert(slickCreate.convert(data)) -> source.extData,
            preData = source.preData,
            table = source.table,
            preRep = source.preRep,
            preShape = source.preShape,
            autoIncRep = source.autoIncRep,
            autoIncShape = source.autoIncShape,
            extRep = sourceCol -> source.extRep,
            extShape = Shape.tuple2Shape(s.mainShape, source.extShape),
            //fmanyGen = source.manyGen,
            subGen = source.subGen,
            autaulCol = source.autaulCol
          )
        }
      } }.toList

      ISWriter(
        extData = (),
        preData = (),
        table = slickCreate.mainCol.owner,
        preRep = (),
        preShape = implicitly[Shape[FlatShapeLevel, Unit, Unit, Unit]],
        autoIncRep = (slickCreate.mainCol.rep: slickCreate.SourceType),
        autoIncShape = slickCreate.mainShape,
        extRep = (),
        extShape = implicitly[Shape[FlatShapeLevel, Unit, Unit, Unit]],
        //TODO 可以去除本字段
        /*fmanyGen = (incData: slickCreate.SlickType) => Future.sequence(staticManyList.map { eachMany =>
          eachMany.staticMany.map { manyList => manyList.map { many =>
            many.proName -> many.gen(slickCreate.convert(incData))
          } }
        }).map(_.flatten.toMap),*/
        subGen = oneToOneSubGen,
        autaulCol = (t: slickCreate.SlickType) => ColumnWithIndex(FsnColumn(columns.column.cols/*, columns.column.proName*/, Option(slickCreate.convert(t): columns.column.DataType)), columns.index) :: Nil
      )
    } else {
      val oneToOneSubGen = oneToOneCreateOpt.map(s => new CWrapTran[Unit] {
        override val table = s.mainCol.owner
        def convert(data: Unit, source: ISlickWriter): ISlickWriter = {
          val sourceCol: s.SourceType = s.mainCol.rep
          if (s.mainCol.owner != source.table) {
            throw new Exception("扩展插入列与该对象的的其他列属于不同的表")
          }
          ISWriter(
            extData = s.convert(columns.column.data.get) -> source.extData,
            preData = source.preData,
            table = source.table,
            preRep = source.preRep,
            preShape = source.preShape,
            autoIncRep = source.autoIncRep,
            autoIncShape = source.autoIncShape,
            extRep = sourceCol -> source.extRep,
            extShape = Shape.tuple2Shape(s.mainShape, source.extShape),
            //fmanyGen = source.manyGen,
            subGen = source.subGen,
            autaulCol = source.autaulCol
          )
        }
      }).toList

      ISWriter(
        extData = (),
        preData = {
          //println(FColumn.find(columns.column) { case e: FProperty[_] => e}.proName)
          //println(columns.column.data)
          slickCreate.reverseConvert(columns.column.data.get)
        },
        table = slickCreate.mainCol.owner,
        preRep = (slickCreate.mainCol.rep: slickCreate.SourceType),
        preShape = slickCreate.mainShape,
        autoIncRep = (),
        autoIncShape = implicitly[Shape[FlatShapeLevel, Unit, Unit, Unit]],
        extRep = (),
        extShape = implicitly[Shape[FlatShapeLevel, Unit, Unit, Unit]],
        /*fmanyGen = (incData: Unit) => Future.sequence(staticManyList.map { eachMany =>
          eachMany.staticMany.map { manyList => manyList.map { many =>
            many.proName -> many.gen(columns.data.get)
          } }
        }).map(_.flatten.toMap),*/
        subGen = oneToOneSubGen,
        autaulCol = (data: Unit) => columns :: Nil
      )
    }
  }
}

trait ISlickMonad {

  implicit def iSlickMonad(implicit ec: ExecutionContext): scalaz.Semigroup[ISlickWriter] = new scalaz.Semigroup[ISlickWriter] {
    def append(f1: ISlickWriter, f2: => ISlickWriter): ISlickWriter = {
      val f2Case = f2
      if (f1.table != f2Case.table) {
        throw new Exception("不能合并不同表的列")
      }
      ISWriter(
        extData = f1.extData -> f2Case.extData,
        preData = f1.preData -> f2Case.preData,
        table = f1.table,
        preRep = f1.preRep -> f2Case.preRep,
        preShape = Shape.tuple2Shape(f1.preShape, f2Case.preShape),
        autoIncRep = f1.autoIncRep -> f2Case.autoIncRep,
        autoIncShape = Shape.tuple2Shape(f1.autoIncShape, f2Case.autoIncShape),
        extRep = f1.extRep -> f2Case.extRep,
        extShape = Shape.tuple2Shape(f1.extShape, f2Case.extShape),
        /*fmanyGen = { s: (f1.IncValue, f2Case.IncValue) => {
          for {
            many1 <- f1.manyGen(s._1)
            many2 <- f2Case.manyGen(s._2)
          } yield {
            many1 ++ many2
          }
        } },*/
        subGen =
        {
          val f1List = f1.subGen.map { eachGen =>
            new CWrapTran[(f1.IncValue, f2Case.IncValue)] {
              val table = eachGen.table
              def convert(data: (f1.IncValue, f2Case.IncValue), source: ISlickWriter): ISlickWriter = {
                eachGen.convert(data._1, source)
              }
            }
          }
          val f2List = f2Case.subGen.map { eachGen =>
            new CWrapTran[(f1.IncValue, f2Case.IncValue)] {
              val table = eachGen.table
              def convert(data: (f1.IncValue, f2Case.IncValue), source: ISlickWriter): ISlickWriter = {
                eachGen.convert(data._2, source)
              }
            }
          }
          f1List ::: f2List
        },
        autaulCol = { s: (f1.IncValue, f2Case.IncValue) =>
          f1.autaulCol(s._1) ::: f2Case.autaulCol(s._2)
        }
      )
    }
  }

}

object InsertWrapDeal extends ISlickMonad {

  import slick.dbio.DBIO
  import slick.lifted.Query

  trait WrapTran {
    val table: RelationalProfile#Table[_]
    def convert(source: ISlickWriter): ISlickWriter
  }

  def parseInsertGen(
    binds: List[(RelationalProfile#Table[_], SlickQueryBindImpl)],
    insertListWrap: List[ColumnWithIndex],
    converts: List[WrapTran]
  )(
    implicit
    ec: ExecutionContext,
    cv: Query[_, String, Seq] => JdbcActionComponent#InsertActionExtensionMethods[String],
    retrieveCv: Query[_, String, Seq] => BasicProfile#StreamingQueryActionExtensionMethods[Seq[String], String]
  ): DBIO[ExecInfo] = {
    //println("5678 " * 100)

    val zipDeal = implicitly[scalaz.Semigroup[ISlickWriter]]

    val wrapList = insertListWrap.map(InCreateConvert.convert)

    val currents = wrapList.groupBy(_.table).filter { case (key, s) => converts.exists(t => key == t.table) }.map { case (key, writers) =>
      writers.reduce { (s, t) => zipDeal.append(s, t) }
    }
    val results = currents.map { eachWrap =>
      val autualWrap = converts.filter(s => s.table == eachWrap.table).foldLeft(eachWrap) { (x, y) =>
        y.convert(x)
      }
      val insertData = autualWrap.preData -> autualWrap.extData
      val bind = binds.find(s => s._1 == autualWrap.table).map(_._2).get
      val preExtQuery = bind.bind(Query(autualWrap.preRep -> autualWrap.extRep)(Shape.tuple2Shape(autualWrap.preShape, autualWrap.extShape)))
      val insertIdQuery = bind.bind(Query(autualWrap.autoIncRep)(autualWrap.autoIncShape))
      /*val insertDBIO: DBIO[autualWrap.IncValue] = {
        cv.asInstanceOf[Query[_, (autualWrap.PreValue, autualWrap.ExtValue), Seq] => JdbcActionComponent#InsertActionExtensionMethods[(autualWrap.PreValue, autualWrap.ExtValue)]](preExtQuery) returning insertIdQuery += insertData
      }*/
      val insertDBIO: DBIO[autualWrap.IncValue] = {
        val newCv = cv.asInstanceOf[Query[_, (autualWrap.PreValue, autualWrap.ExtValue), Seq] => JdbcActionComponent#InsertActionExtensionMethods[(autualWrap.PreValue, autualWrap.ExtValue)]]
        if (SlickUtils.isShapeEmpty(autualWrap.autoIncShape)) {
          val newQueryCv = retrieveCv.asInstanceOf[Query[_, autualWrap.IncValue, Seq] => BasicProfile#StreamingQueryActionExtensionMethods[Seq[autualWrap.IncValue], autualWrap.IncValue]]
          for {
            iDBIO <- newCv(preExtQuery) += insertData
            insertResult <- newQueryCv(insertIdQuery).result.head
          } yield {
            insertResult
          }
        } else {
          newCv(preExtQuery) returning insertIdQuery += insertData
        }
      }
      for {
        incData <- insertDBIO
        //many <- DBIO.from(autualWrap.manyGen(incData))
        listToMerge = autualWrap.autaulCol(incData)
        //aa = println(listToMerge)
        subs = autualWrap.subGen.map { u =>
          new WrapTran {
            override val table = u.table
            override def convert(source: ISlickWriter): ISlickWriter = {
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
  }

  def parseInsertWithIndex(
    binds: List[(RelationalProfile#Table[_], SlickQueryBindImpl)],
    insertListWrap: List[ColumnWithIndex]
  )(implicit
    ec: ExecutionContext,
    cv: Query[_, String, Seq] => JdbcActionComponent#InsertActionExtensionMethods[String],
    retrieveCv: Query[_, String, Seq] => BasicProfile#StreamingQueryActionExtensionMethods[Seq[String], String]
  ): DBIO[ExecInfo] = {
    //println("9101112 " * 100)

    val zipDeal = implicitly[scalaz.Semigroup[ISlickWriter]]

    val wrapList = insertListWrap.map(InCreateConvert.convert)
    val dealInserts = wrapList.groupBy(_.table).map(_._2).map(s => s.reduce { (u, v) => zipDeal.append(u, v) }).toList
    val tables = dealInserts.flatMap { t => t.subGen.map(_.table) }
    val currents = dealInserts.filter { s =>
      tables.forall(t => t != s.table)
    }
    val results = currents.map { eachWrap =>
      /*val autualWrap = converts.filter(s => s.table == eachWrap.table).foldLeft(eachWrap) { (x, y) =>
        y.convert(x: y.AbsWrap[x.PreRep, x.PreValue, x.PreTarget, x.IncRep, x.IncValue, x.IncTarget])
      }*/
      val autualWrap = eachWrap
      val insertData = autualWrap.preData -> autualWrap.extData
      val bind = binds.find(s => s._1 == autualWrap.table).map(_._2).get
      val preExtQuery = bind.bind(Query(autualWrap.preRep -> autualWrap.extRep)(Shape.tuple2Shape(autualWrap.preShape, autualWrap.extShape)))
      val insertIdQuery = bind.bind(Query(autualWrap.autoIncRep)(autualWrap.autoIncShape))
      val insertDBIO: DBIO[autualWrap.IncValue] = {
        val newCv = cv.asInstanceOf[Query[_, (autualWrap.PreValue, autualWrap.ExtValue), Seq] => JdbcActionComponent#InsertActionExtensionMethods[(autualWrap.PreValue, autualWrap.ExtValue)]]
        if (SlickUtils.isShapeEmpty(autualWrap.autoIncShape)) {
          val newQueryCv = retrieveCv.asInstanceOf[Query[_, autualWrap.IncValue, Seq] => BasicProfile#StreamingQueryActionExtensionMethods[Seq[autualWrap.IncValue], autualWrap.IncValue]]
          for {
            iDBIO <- newCv(preExtQuery) += insertData
            insertResult <- newQueryCv(insertIdQuery).result.head
          } yield {
            insertResult
          }
        } else {
          newCv(preExtQuery) returning insertIdQuery += insertData
        }
      }

      for {
        incData <- insertDBIO
        //bb = println("13141516 " * 100)
        //many <- DBIO.from(autualWrap.manyGen(insertData))
        listToMerge = autualWrap.autaulCol(incData)
        subs = autualWrap.subGen.map { u =>
          new WrapTran {
            override val table = u.table
            override def convert(source: ISlickWriter): ISlickWriter = {
              u.convert(incData, source)
            }
          }
        }
        subResult <- parseInsertGen(binds, insertListWrap, subs)
      //subResult <- parseInsertGen(binds, dealInserts, subs)(ec, cv).apply(data)
        //subResult <- parseInsertGen(binds, mergeList(insertListWrap, listToMerge), subs)
      } yield {
        //ExecInfo(subResult.effectRows + 1, subResult.columns ::: listToMerge)//UpdateStaticManyInfo(subResult.effectRows + 1, subResult.many ++ Map())
        ExecInfo(subResult.effectRows + 1, subResult.columns ::: listToMerge)//UpdateStaticManyInfo(subResult.effectRows + 1, subResult.many ++ Map())
      }
    }
    results.foldLeft(DBIO.successful(ExecInfo(0, Nil)): DBIO[ExecInfo]) { (s, t) =>
      (for {
          s1 <- s
          t1 <- t
      } yield {
        //println(s1.columns, t1.columns)
        //println(ExecInfo(s1.effectRows + t1.effectRows, deepMerge(s1.columns, t1.columns)))
        //ExecInfo(s1.effectRows + t1.effectRows, mergeList(s1.columns, t1.columns))
        ExecInfo(s1.effectRows + t1.effectRows, s1.columns ::: t1.columns)
      })
    }
  }

  def parseInsert(
    binds: List[(RelationalProfile#Table[_], SlickQueryBindImpl)],
    insertListWrap: List[FColumn]
  )(implicit
    ec: ExecutionContext,
    cv: Query[_, String, Seq] => JdbcActionComponent#InsertActionExtensionMethods[String],
    retrieveCv: Query[_, String, Seq] => BasicProfile#StreamingQueryActionExtensionMethods[Seq[String], String]
  ): DBIO[ExecInfo] = {
    parseInsertWithIndex(binds, insertListWrap.zipWithIndex.map(ColumnWithIndex.tupled)).map { s => s.copy(columns = s.columns.sortBy(_.index)) }
  }

}*/