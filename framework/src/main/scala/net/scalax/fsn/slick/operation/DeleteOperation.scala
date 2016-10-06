package net.scalax.fsn.slick.operation

import net.scalax.fsn.core.FColumn
import net.scalax.fsn.slick.atomic.{OneToOneRetrieve, SlickDelete}
import net.scalax.fsn.slick.helpers.{FilterColumnGen, ListAnyShape, SlickQueryBindImpl}
import net.scalax.fsn.slick.model.UpdateStaticManyInfo
import slick.dbio.DBIO
import slick.jdbc.JdbcActionComponent
import slick.lifted._
import slick.relational.RelationalProfile

import scala.language.existentials
import scala.concurrent.ExecutionContext

trait DeleteTran2 {
  val table: RelationalProfile#Table[_]
  def convert(source: DeleteQuery): DeleteQuery
}

trait DeleteQuery {

  val bind: SlickQueryBindImpl
  val cols: List[Any]
  val shapes: List[Shape[_ <: FlatShapeLevel, _, _, _]]
  val filters: List[FilterColumnGen[Seq[Any]]]

}

trait DSlickWriter2 {

  type MainSColumn
  type MainDColumn
  type MainTColumn

  val mainCol: MainSColumn

  val mainShape: Shape[_ <: FlatShapeLevel, MainSColumn, MainDColumn, MainTColumn]

  val table: RelationalProfile#Table[_]
  val primaryGen: Option[FilterColumnGen[MainTColumn]]

  val subGen: Option[DeleteTran2]

}

case class DSWriter2[MS, MD, MT, D](
                                    override val mainCol: MS,
                                    override val mainShape: Shape[_ <: FlatShapeLevel, MS, MD, MT],
                                    override val table: RelationalProfile#Table[_],
                                    override val primaryGen: Option[FilterColumnGen[MT]],
                                    override val subGen: Option[DeleteTran2]
                                  ) extends DSlickWriter2 {

  override type MainSColumn = MS
  override type MainDColumn = MD
  override type MainTColumn = MT

}

object InDeleteConvert2 {

  def convert(columns: FColumn)(implicit ec: ExecutionContext): DSlickWriter2 = {
    val slickDelete = FColumn.find(columns)({ case s: SlickDelete[columns.DataType] => s })
    val oneToDeleteOpt = FColumn.findOpt(columns)({ case s: OneToOneRetrieve[columns.DataType] => s })
    val subGenOpt = oneToDeleteOpt.map { oneToOneDelete =>
      new DeleteTran2 {
        override val table = oneToOneDelete.owner

        override def convert(source: DeleteQuery): DeleteQuery = {
          new DeleteQuery {
            override val bind = source.bind
            override val cols = source.cols ::: oneToOneDelete.mainCol :: Nil
            override val shapes = source.shapes ::: oneToOneDelete.mainShape :: Nil
            override val filters = source.filters ::: {
              val index = cols.indexOf(oneToOneDelete.mainCol)
              new FilterColumnGen[Seq[Any]] {
                override type BooleanTypeRep = oneToOneDelete.primaryGen.BooleanTypeRep
                override val dataToCondition = { cols: Seq[Any] =>
                  val col = cols(index).asInstanceOf[oneToOneDelete.TargetType]
                  val slickData = oneToOneDelete.filterConvert(columns.data.get)
                  oneToOneDelete.primaryGen.dataToCondition(col)(slickData)
                }
                override val wt = oneToOneDelete.primaryGen.wt
              }
            } :: Nil
          }
        }
      }
    }
    DSWriter2(
      mainCol = slickDelete.mainCol,
      mainShape = slickDelete.mainShape,
      table = slickDelete.owner,
      primaryGen = slickDelete.primaryGen.map { eachPri => (new FilterColumnGen[slickDelete.TargetType] {
        override type BooleanTypeRep = eachPri.BooleanTypeRep
        override val dataToCondition = { sourceCol: slickDelete.TargetType =>
          eachPri.dataToCondition(sourceCol)(
            slickDelete.filterConvert(columns.data.get)
          )
        }
        override val wt = eachPri.wt
      }) },
      subGen = subGenOpt
    )
  }

}

object DeleteOperation {

  def parseInsertGen(
                      binds: List[(RelationalProfile#Table[_], SlickQueryBindImpl)],
                      updateList: List[FColumn],
                      converts: List[DeleteTran2]
                    )(
                      implicit
                      ec: ExecutionContext,
                      deleteConV: Query[RelationalProfile#Table[_], _, Seq] => JdbcActionComponent#DeleteActionExtensionMethods
  ): DBIO[UpdateStaticManyInfo] = {
    val wrapList = updateList.map(InDeleteConvert2.convert)

    val currents = wrapList.groupBy(_.table).filter { case (key, s) => converts.exists(t => key == t.table) }
    val results = currents.map { case (table, eachWrap) =>
      val initDeleteQuery: DeleteQuery = new DeleteQuery {
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
      val convertRetrieveQuery = converts.filter(_.table == table).foldLeft(initDeleteQuery) { (x, y) =>
        y.convert(x)
      }
      val query = Query(convertRetrieveQuery.cols)(new ListAnyShape[FlatShapeLevel](convertRetrieveQuery.shapes))
      val bindQuery = convertRetrieveQuery.bind.bind(query)
      val filterQuery = convertRetrieveQuery.filters.foldLeft(bindQuery) { (x, y) =>
        x.filter(s => y.dataToCondition(s))(y.wt)
      }
      val updateDBIO = filterQuery.asInstanceOf[Query[RelationalProfile#Table[_], _, Seq]].delete
      for {
        effectRows <- updateDBIO
        subs = eachWrap.map(_.subGen.toList).flatten
        subResult <- parseInsertGen(binds, updateList, subs)
      } yield {
        UpdateStaticManyInfo(effectRows + subResult.effectRows, subResult.many)
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
                   binds: List[(RelationalProfile#Table[_], SlickQueryBindImpl)],
                   updateList: List[FColumn]
                 )(
                   implicit
                   ec: ExecutionContext,
                   deleteConV: Query[RelationalProfile#Table[_], _, Seq] => JdbcActionComponent#DeleteActionExtensionMethods
  ): DBIO[UpdateStaticManyInfo] = {
    val wrapList = updateList.map(InDeleteConvert2.convert)

    val subGensTables = wrapList.flatMap { t => t.subGen.toList.map(_.table) }
    val currents = wrapList.groupBy(_.table).filter { case (key, s) => subGensTables.forall(t => key != t) }
    val results = currents.map { case (table, eachWrap) =>
      val initDeleteQuery: DeleteQuery = new DeleteQuery {
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
      val convertRetrieveQuery = initDeleteQuery
      val query = Query(convertRetrieveQuery.cols)(new ListAnyShape[FlatShapeLevel](convertRetrieveQuery.shapes))
      val bindQuery = convertRetrieveQuery.bind.bind(query)
      val filterQuery = convertRetrieveQuery.filters.foldLeft(bindQuery) { (x, y) =>
        x.filter(s => y.dataToCondition(s))(y.wt)
      }
      val updateDBIO = filterQuery.asInstanceOf[Query[RelationalProfile#Table[_], _, Seq]].delete
      for {
        effectRows <- updateDBIO
        subs = eachWrap.map(_.subGen.toList).flatten
        subResult <- parseInsertGen(binds, updateList, subs)
      } yield {
        UpdateStaticManyInfo(effectRows + subResult.effectRows, subResult.many)
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
}