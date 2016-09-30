/*package indicator.rw.utils.rw2

import aaaa.FilterColumnGen
import indicator.rw.utils.SlickQueryBindImpl
import indicator.rw.utils.rw.{FProperty, OneToOneUpdate, SlickUpdate}
import net.scalax.fsn.core.FColumn
import net.scalax.fsn.json_slick.UpdateStaticManyInfo
import slick.dbio.DBIO
import slick.jdbc.JdbcActionComponent
import slick.lifted.{FlatShapeLevel, Query, Shape}
import slick.relational.RelationalProfile

import scala.language.existentials
import scala.concurrent.ExecutionContext

trait UUpdateTran {
  val table: RelationalProfile#Table[_]
  def convert(source: USlickWriter): USlickWriter
}

trait USlickWriter {

  type MainSColumn
  type MainDColumn
  type MainTColumn

  type UpdateSColumn
  type UpdateDColumn
  type UpdateTColumn

  val mainCol: MainSColumn
  val columnConvert: MainTColumn => UpdateSColumn

  val mainShape: Shape[_ <: FlatShapeLevel, MainSColumn, MainDColumn, MainTColumn]
  val updateShape: Shape[_ <: FlatShapeLevel, UpdateSColumn, UpdateDColumn, UpdateTColumn]

  val table: RelationalProfile#Table[_]

  val effect: UpdateDColumn

  val primaryGen: List[FilterColumnGen[MainTColumn]]

  val subGen: List[UUpdateTran]

}

case class USWriter[MS, MD, MT, D, US, UD, UT](
                                                override val mainCol: MS,
                                                override val mainShape: Shape[_ <: FlatShapeLevel, MS, MD, MT],
                                                override val updateShape: Shape[_ <: FlatShapeLevel, US, UD, UT],
                                                override val columnConvert: MT => US,
                                                override val table: RelationalProfile#Table[_],
                                                //override val staticManyMap: Future[Map[String, QueryJsonInfo]],
                                                override val effect: UD,
                                                override val primaryGen: List[FilterColumnGen[MT]],
                                                override val subGen: List[UUpdateTran]
                                              ) extends USlickWriter {

  override type MainSColumn = MS
  override type MainDColumn = MD
  override type MainTColumn = MT

  override type UpdateSColumn = US
  override type UpdateDColumn = UD
  override type UpdateTColumn = UT

}

trait UJsonSlickMonad {

  import scalaz._

  implicit def pSlickReaderZip(implicit ec: ExecutionContext): Semigroup[USlickWriter] = new Semigroup[USlickWriter] {

    override def append(f1: USlickWriter, f2: => USlickWriter): USlickWriter = {
      val f2Case = f2
      val newMainShape = Shape.tuple2Shape(f1.mainShape, f2Case.mainShape)
      val newUpdateShape = Shape.tuple2Shape(f1.updateShape, f2Case.updateShape)
      val appendPrimaryGen: List[FilterColumnGen[(f1.MainTColumn, f2Case.MainTColumn)]] = {
        val thisWrappers = f1.primaryGen.map { s =>
          new FilterColumnGen[(f1.MainTColumn, f2Case.MainTColumn)] {
            override type BooleanTypeRep = s.BooleanTypeRep
            override val dataToCondition = { col: (f1.MainTColumn, f2Case.MainTColumn) =>
              s.dataToCondition(col._1)
            }
            override val wt = s.wt
          }
        }

        val appendWrappers = f2Case.primaryGen.map { s =>
          new FilterColumnGen[(f1.MainTColumn, f2Case.MainTColumn)] {
            override type BooleanTypeRep = s.BooleanTypeRep
            override val dataToCondition = { col: (f1.MainTColumn, f2Case.MainTColumn) =>
              s.dataToCondition(col._2)
            }
            override val wt = s.wt
          }
        }

        thisWrappers ::: appendWrappers
      }

      val f1IWraps = f1.subGen
      val f2IWraps = f2Case.subGen

      //TODO 看是否需要对表进行唯一判断
      val newWraps: List[UUpdateTran] = if (f1IWraps.isEmpty && f2IWraps.isEmpty) {
        Nil
      } else {
        val tables = (f1IWraps.map(_.table) ::: f2IWraps.map(_.table)).distinct
        tables match {
          case headTable :: Nil =>
            val f1NewWraps = f1IWraps.map { eachWrap =>
              new UUpdateTran {
                override val table = headTable
                override def convert(source: USlickWriter): USlickWriter = {
                  eachWrap.convert(source)
                }
              }
            }
            val f2NewWraps = f2IWraps.map { eachWrap =>
              new UUpdateTran {
                override val table = headTable
                override def convert(source: USlickWriter): USlickWriter = {
                  eachWrap.convert(source)
                }
              }
            }
            f1NewWraps ::: f2NewWraps
          case _ =>
            throw new Exception("要合并的 2 个 reader 来自不同的表")
        }
      }

      USWriter(
        mainCol = f1.mainCol -> f2Case.mainCol,
        table = (f1.table :: f2Case.table :: Nil).distinct match {
          case headTable :: Nil =>
            headTable
          case _ =>
            throw new Exception("要合并的 2 个 reader 来自不同的表11111111")
        },
        columnConvert = { s1: (f1.MainTColumn, f2Case.MainTColumn) =>
          f1.columnConvert(s1._1) -> f2Case.columnConvert(s1._2)
        },
        mainShape = newMainShape,
        updateShape = newUpdateShape,
        effect = f1.effect -> f2Case.effect,
        primaryGen = appendPrimaryGen,
        subGen = newWraps
      )
    }

  }

}

object InUpdateConvert extends UJsonSlickMonad {

  def convert(columns: FColumn)(implicit ec: ExecutionContext): USlickWriter = {
    val slickWriter = FColumn.find(columns)({ case s: SlickUpdate[columns.DataType] => s })
    val oneToOneRetrieveOpt = FColumn.findOpt(columns)({ case s: OneToOneUpdate[columns.DataType] => s })

    val uSlickSubGen = oneToOneRetrieveOpt match {
      case Some(oneToOneRetrieve) =>
        List(new UUpdateTran {
          override val table = oneToOneRetrieve.mainCol.owner
          override def convert(source: USlickWriter): USlickWriter = {
            val usWriter = USWriter(
              mainCol = source.mainCol -> (oneToOneRetrieve.mainCol.rep: oneToOneRetrieve.SourceType),
              mainShape = Shape.tuple2Shape(source.mainShape, oneToOneRetrieve.mainShape),
              updateShape = Shape.tuple2Shape(source.updateShape, oneToOneRetrieve.mainShape.packedShape),
              columnConvert = (s: (source.MainTColumn, oneToOneRetrieve.TargetType)) => source.columnConvert(s._1) -> s._2,
              table = source.table,
              effect = source.effect -> oneToOneRetrieve.convert(columns.data.get),
              primaryGen = source.primaryGen.map { eachPrimaryGen =>
                new FilterColumnGen[(source.MainTColumn, oneToOneRetrieve.TargetType)] {
                  override type BooleanTypeRep = eachPrimaryGen.BooleanTypeRep
                  override val dataToCondition = { filterCol: (source.MainTColumn, oneToOneRetrieve.TargetType) =>
                    eachPrimaryGen.dataToCondition(filterCol._1)
                  }
                  override val wt = eachPrimaryGen.wt
                }
              } ::: {
                new FilterColumnGen[(source.MainTColumn, oneToOneRetrieve.TargetType)] {
                  override type BooleanTypeRep = oneToOneRetrieve.primaryGen.BooleanTypeRep
                  override val dataToCondition = { filterCol: (source.MainTColumn, oneToOneRetrieve.TargetType) =>
                    oneToOneRetrieve.primaryGen.dataToCondition(filterCol._2)(
                      oneToOneRetrieve.filterConvert(columns.data.get)
                    )
                  }
                  override val wt = oneToOneRetrieve.primaryGen.wt
                }
              } :: Nil,
              subGen = source.subGen
            )
            usWriter
          }
        })
      case _ => List.empty[UUpdateTran]
    }
    val uSlickWriter = slickWriter.primaryGen match {
      case Some(eachPri) =>
        USWriter(
          mainCol = (slickWriter.mainCol.rep: slickWriter.SourceType),
          mainShape = slickWriter.mainShape,
          updateShape = implicitly[Shape[FlatShapeLevel, Unit, Unit, Unit]],
          columnConvert = (s: slickWriter.TargetType) => (),
          table = slickWriter.mainCol.owner,
          //staticManyMap = InStaticManyConvert.convert2Query(columns),
          effect = (),
          primaryGen = List(new FilterColumnGen[slickWriter.TargetType] {
            override type BooleanTypeRep = eachPri.BooleanTypeRep
            override val dataToCondition = { sourceCol: slickWriter.TargetType =>
              eachPri.dataToCondition(sourceCol)(
                slickWriter.filterConvert(columns.data.get)
              )
            }
            override val wt = eachPri.wt
          }),
          subGen = uSlickSubGen
        )
      case _ =>
        USWriter(
          mainCol = (slickWriter.mainCol.rep: slickWriter.SourceType),
          mainShape = slickWriter.mainShape,
          updateShape = slickWriter.mainShape.packedShape,
          columnConvert = (s: slickWriter.TargetType) => s,
          table = slickWriter.mainCol.owner,
          effect = {
            //println(FColumn.find(columns) { case e: FProperty[_] => e}.proName)
            //println(columns.data)
            slickWriter.convert(columns.data.get)
          },
          primaryGen = List.empty[FilterColumnGen[slickWriter.TargetType]],
          subGen = uSlickSubGen
        )
    }
    uSlickWriter
  }

}

object UpdateWrapDeal extends UJsonSlickMonad {

  def parseInsertGen(
    binds: List[(RelationalProfile#Table[_], SlickQueryBindImpl)],
    insertListWrap: List[USlickWriter],
    converts: List[UUpdateTran]
  )(
    implicit
    ec: ExecutionContext,
    updateConV: Query[_, String, Seq] => JdbcActionComponent#UpdateActionExtensionMethods[String]
  ): DBIO[UpdateStaticManyInfo] = {
    val currents = insertListWrap.filter(s => converts.exists(t => s.table == t.table))
    val results = currents.map { eachWrap =>
      val autualWrap = converts.filter(s => s.table == eachWrap.table).foldLeft(eachWrap) { (x, y) =>
        y.convert(x)
      }
      val bind = binds.find(s => s._1 == autualWrap.table).map(_._2).get
      val preQuery = bind.bind(Query(autualWrap.mainCol)(autualWrap.mainShape))

      val filterQuery = autualWrap.primaryGen.foldLeft(preQuery) { (query, gen) =>
        query.filter(filterCol => gen.dataToCondition(filterCol))(gen.wt)
      }
      val updateQuery = filterQuery.map(autualWrap.columnConvert)(autualWrap.updateShape)
      val updateData = autualWrap.effect

      val convert = updateConV.asInstanceOf[Query[_, autualWrap.UpdateDColumn, Seq] => JdbcActionComponent#UpdateActionExtensionMethods[autualWrap.UpdateDColumn]]
      for {
        effectRows <- convert(updateQuery).update(updateData)
        subs = autualWrap.subGen
        subResult <- parseInsertGen(binds, insertListWrap, subs)
      } yield {
        UpdateStaticManyInfo(effectRows + subResult.effectRows, subResult.many/*++ many*/)
      }
    }
    results.foldLeft(DBIO.successful(UpdateStaticManyInfo(0, Map())): DBIO[UpdateStaticManyInfo]) { (s, t) =>
      (for {
        s1 <- s
        t1 <- t
      } yield {
        UpdateStaticManyInfo(s1.effectRows + t1.effectRows, s1.many ++ t1.many)
      })
    }
  }

  def parseInsert(
    insertQuerytWrap: List[(RelationalProfile#Table[_], SlickQueryBindImpl)],
    columns: List[FColumn]
  )(
    implicit
    ec: ExecutionContext,
    updateConV: Query[_, String, Seq] => JdbcActionComponent#UpdateActionExtensionMethods[String]
  ): DBIO[UpdateStaticManyInfo] = {
    val zipDeal = implicitly[scalaz.Semigroup[USlickWriter]]

    val uSlickWriters = columns.map(InUpdateConvert.convert(_))

    val dealInserts = uSlickWriters.groupBy(_.table).map(_._2).map(s => s.reduce { (u, v) => zipDeal.append(u, v): USlickWriter }).toList
    val tables = dealInserts.flatMap { t => t.subGen.map(_.table) }
    val currents = dealInserts.filter { s =>
      tables.forall(t => t != s.table)
    }
    val results = currents.map { eachWrap =>
      val autualWrap = eachWrap
      val bind = insertQuerytWrap.find(s => s._1 == autualWrap.table).map(_._2).get
      val preQuery = bind.bind(Query(autualWrap.mainCol)(autualWrap.mainShape))

      val filterQuery = autualWrap.primaryGen.foldLeft(preQuery) { (query, gen) =>
        query.filter(filterCol => gen.dataToCondition(filterCol))(gen.wt)
      }
      val updateQuery = filterQuery.map(autualWrap.columnConvert)(autualWrap.updateShape)

      val updateData = autualWrap.effect

      val insertDBIO: DBIO[Int] = {
        val convert = updateConV.asInstanceOf[Query[_, autualWrap.UpdateDColumn, Seq] => JdbcActionComponent#UpdateActionExtensionMethods[autualWrap.UpdateDColumn]]
        convert(updateQuery).update(updateData)
      }

      for {
        preData <- insertDBIO
        subs = autualWrap.subGen
        subResult <- parseInsertGen(insertQuerytWrap, dealInserts, subs)(ec, updateConV)
      } yield {
        UpdateStaticManyInfo(preData + subResult.effectRows, subResult.many)
      }
    }
    results.foldLeft(DBIO.successful(UpdateStaticManyInfo(0, Map())): DBIO[UpdateStaticManyInfo]) { (s, t) =>
      (for {
        s1 <- s
        t1 <- t
      } yield {
        UpdateStaticManyInfo(s1.effectRows + t1.effectRows, s1.many ++ t1.many)
      })
    }
  }
}*/