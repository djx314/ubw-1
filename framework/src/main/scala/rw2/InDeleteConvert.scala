/*package indicator.rw.utils.rw2

import aaaa.FilterColumnGen
import indicator.rw.utils.SlickQueryBindImpl
import indicator.rw.utils.rw.{OneToOneRetrieve, SlickDelete}
import net.scalax.fsn.core.FColumn
import net.scalax.fsn.json_slick.{QueryJsonInfo, UpdateStaticManyInfo}
import slick.dbio.DBIO
import slick.jdbc.JdbcActionComponent
import slick.lifted._
import slick.relational.RelationalProfile

import scala.language.existentials
import scala.concurrent.{ExecutionContext, Future}
import scalaz.Semigroup

trait DeleteTran {
  val table: RelationalProfile#Table[_]
  def convert(source: DSlickWriter): DSlickWriter
}

trait DSlickWriter {

  type MainSColumn
  type MainDColumn
  type MainTColumn

  val mainCol: MainSColumn

  val mainShape: Shape[_ <: FlatShapeLevel, MainSColumn, MainDColumn, MainTColumn]

  val table: RelationalProfile#Table[_]
  val primaryGen: List[FilterColumnGen[MainTColumn]]

  val subGen: List[DeleteTran]

}

case class DSWriter[MS, MD, MT, D](
                                    override val mainCol: MS,
                                    override val mainShape: Shape[_ <: FlatShapeLevel, MS, MD, MT],
                                    override val table: RelationalProfile#Table[_],
                                    override val primaryGen: List[FilterColumnGen[MT]],
                                    override val subGen: List[DeleteTran]
                                  ) extends DSlickWriter {

  override type MainSColumn = MS
  override type MainDColumn = MD
  override type MainTColumn = MT

}

object InDeleteConvert {

  def convert(columns: List[FColumn])(implicit ec: ExecutionContext): List[DSlickWriter] = {
    columns.zipWithIndex.map { case (eachColumn, index) =>
      val slickDelete = FColumn.find(eachColumn)({ case s: SlickDelete[eachColumn.DataType] => s })
      val oneToOneRetrieveOpt = FColumn.findOpt(eachColumn)({ case s: OneToOneRetrieve[eachColumn.DataType] => s })
      val uSlickSubGen = oneToOneRetrieveOpt match {
        case Some(oneToOneRetrieve) =>
          List(new DeleteTran {
            override val table = oneToOneRetrieve.mainCol.owner
            override def convert(source: DSlickWriter): DSlickWriter = {
              val usWriter = DSWriter(
                mainCol = source.mainCol -> (oneToOneRetrieve.mainCol.rep: oneToOneRetrieve.SourceType),
                mainShape = Shape.tuple2Shape(source.mainShape, oneToOneRetrieve.mainShape),
                table = source.table,
                primaryGen = source.primaryGen.map { t =>
                  new FilterColumnGen[(source.MainTColumn, oneToOneRetrieve.TargetType)] {
                    override type BooleanTypeRep = t.BooleanTypeRep
                    override val dataToCondition = { filterCol: (source.MainTColumn, oneToOneRetrieve.TargetType) =>
                      t.dataToCondition(filterCol._1)
                    }
                    override val wt = t.wt
                  }
                } ::: {
                  new FilterColumnGen[(source.MainTColumn, oneToOneRetrieve.TargetType)] {
                    override type BooleanTypeRep = oneToOneRetrieve.primaryGen.BooleanTypeRep
                    override val dataToCondition = { filterCol: (source.MainTColumn, oneToOneRetrieve.TargetType) =>
                      oneToOneRetrieve.primaryGen.dataToCondition(filterCol._2)(
                        oneToOneRetrieve.filterConvert(eachColumn.data.get)
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
        case _ => List.empty[DeleteTran]
      }

      val dSlickDelete =
        slickDelete.primaryGen match {
          case Some(eachPri) =>
            DSWriter(
              mainCol = (slickDelete.mainCol.rep: slickDelete.SourceType),
              mainShape = slickDelete.mainShape,
              table = slickDelete.mainCol.owner,
              primaryGen = List(new FilterColumnGen[slickDelete.TargetType] {
                override type BooleanTypeRep = eachPri.BooleanTypeRep
                override val dataToCondition = (sourceCol: slickDelete.TargetType) => {
                  eachPri.dataToCondition(sourceCol)(
                    slickDelete.filterConvert(eachColumn.data.get)
                  )
                }
                override val wt = eachPri.wt
              }),
              subGen = uSlickSubGen
            )
          case _ =>
            DSWriter(
              mainCol = (slickDelete.mainCol.rep: slickDelete.SourceType),
              mainShape = slickDelete.mainShape,
              table = slickDelete.mainCol.owner,
              primaryGen = List.empty[FilterColumnGen[slickDelete.TargetType]],
              subGen = uSlickSubGen
            )
        }
      dSlickDelete
    }
  }

}

trait DJsonSlickMonad {

  implicit def dJsonSlick(implicit ec: ExecutionContext): Semigroup[DSlickWriter] = new Semigroup[DSlickWriter] {
    def append(f1: DSlickWriter, f2: => DSlickWriter): DSlickWriter = {
      val f2Case = f2
      DSWriter(
        mainCol = f1.mainCol -> f2Case.mainCol,
        mainShape = Shape.tuple2Shape(f1.mainShape, f2Case.mainShape),
        table = (f1.table :: f2Case.table :: Nil).distinct match {
          case headTable :: Nil =>
            headTable
          case _ =>
            throw new Exception("要合并的 2 个 reader 来自不同的表11111111")
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
        subGen = f1.subGen ::: f2Case.subGen
      )
    }
  }

}

object DeleteWrapDeal extends DJsonSlickMonad {

  def parseInsertGen(
    binds: List[(RelationalProfile#Table[_], SlickQueryBindImpl)],
    insertListWrap: List[DSlickWriter],
    converts: List[DeleteTran]
  )(
    implicit
    ec: ExecutionContext,
    deleteConV: Query[RelationalProfile#Table[_], _, Seq] => JdbcActionComponent#DeleteActionExtensionMethods
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
      for {
        effectRows <- filterQuery.asInstanceOf[Query[RelationalProfile#Table[_], _, Seq]].delete
        subs: List[DeleteTran] = autualWrap.subGen
        subResult <- parseInsertGen(binds, insertListWrap, subs)
      } yield {
        UpdateStaticManyInfo(effectRows + subResult.effectRows, subResult.many)
      }
    }
    results.foldLeft(slick.dbio.DBIO.successful(UpdateStaticManyInfo(0, Map())): slick.dbio.DBIO[UpdateStaticManyInfo]) { (s, t) =>
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
    deleteConV: Query[RelationalProfile#Table[_], _, Seq] => JdbcActionComponent#DeleteActionExtensionMethods
  ): DBIO[UpdateStaticManyInfo] = {
    val zipDeal = implicitly[scalaz.Semigroup[DSlickWriter]]

    val dSlickWriters = InDeleteConvert.convert(columns)
    val dealInserts = dSlickWriters.groupBy(_.table).map(_._2).map(s => s.reduce { (u, v) => zipDeal.append(u, v): DSlickWriter }).toList
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

      val insertDBIO: DBIO[Int] = {
        filterQuery.asInstanceOf[Query[RelationalProfile#Table[_], _, Seq]].delete
      }

      for {
        preData <- insertDBIO
        subs: List[DeleteTran] = autualWrap.subGen
        subResult <- parseInsertGen(insertQuerytWrap, dealInserts, subs)(ec, deleteConV)
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