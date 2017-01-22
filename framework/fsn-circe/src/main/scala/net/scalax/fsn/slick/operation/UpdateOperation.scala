package net.scalax.fsn.slick.operation

import net.scalax.fsn.core._
import net.scalax.fsn.slick.atomic.{OneToOneUpdate, SlickUpdate}
import net.scalax.fsn.slick.helpers.{FilterColumnGen, ListAnyShape, SlickQueryBindImpl}
import net.scalax.fsn.slick.model.UpdateStaticManyInfo
import slick.dbio.DBIO
import slick.jdbc.JdbcActionComponent
import slick.lifted.{FlatShapeLevel, Query, Shape}
import slick.relational.RelationalProfile

import scala.language.existentials
import scala.concurrent.ExecutionContext

import shapeless._

trait UpdateQuery {

  val bind: SlickQueryBindImpl
  val cols: List[Any]
  val shapes: List[Shape[_ <: FlatShapeLevel, _, _, _]]
  val filters: List[FilterColumnGen[Seq[Any]]]

  val updateIndices: List[Int]
  val updateShapes: List[Shape[_ <: FlatShapeLevel, _, _, _]]
  val updateData: List[Any]

}

trait UUpdateTran2 {
  val table: Any
  def convert(source: UpdateQuery): UpdateQuery
}

trait USlickWriter2 {

  type MainSColumn
  type MainDColumn
  type MainTColumn

  val mainCol: MainSColumn

  val mainShape: Shape[_ <: FlatShapeLevel, MainSColumn, MainDColumn, MainTColumn]

  val table: Any

  val data: MainDColumn

  val primaryGen: Option[FilterColumnGen[MainTColumn]]

  val subGen: Option[UUpdateTran2]

}

case class USWriter2[MS, MD, MT](
  override val mainCol: MS,
  override val mainShape: Shape[_ <: FlatShapeLevel, MS, MD, MT],
  override val table: Any,
  override val data: MD,
  override val primaryGen: Option[FilterColumnGen[MT]],
  override val subGen: Option[UUpdateTran2]
) extends USlickWriter2 {

  override type MainSColumn = MS
  override type MainDColumn = MD
  override type MainTColumn = MT

}

object InUpdateConvert2 extends FAtomicGenHelper with FAtomicShapeHelper {

    def updateGen(
      implicit
      ec: ExecutionContext,
      updateConV: Query[_, Seq[Any], Seq] => JdbcActionComponent#UpdateActionExtensionMethods[Seq[Any]]
    ): FPileSyntax.PileGen[Option, List[(Any, SlickQueryBindImpl)] => DBIO[UpdateStaticManyInfo]] = {
    FPile.transformTreeList { path =>
      FAtomicQuery(needAtomic[SlickUpdate] :: needAtomicOpt[OneToOneUpdate] :: HNil)
        .mapToOption(path) { case (slickWriter :: oneToOneUpdateOpt :: HNil, data) => {
          //val slickWriter = FColumn.find(columns)({ case s: SlickUpdate[columns.DataType] => s })
          //val oneToOneUpdateOpt = FColumn.findOpt(columns)({ case s: OneToOneUpdate[columns.DataType] => s })
          val uSlickSubGen = oneToOneUpdateOpt.map { oneToOneUpdate =>
            new UUpdateTran2 {
              override val table = oneToOneUpdate.owner
              override def convert(source: UpdateQuery): UpdateQuery = {
                new UpdateQuery {
                  override val bind = source.bind
                  override val cols = source.cols ::: oneToOneUpdate.mainCol :: Nil
                  override val shapes = source.shapes ::: oneToOneUpdate.mainShape :: Nil
                  override val filters = source.filters ::: {
                    val index = cols.indexOf(oneToOneUpdate.mainCol)
                    new FilterColumnGen[Seq[Any]] {
                      override type BooleanTypeRep = oneToOneUpdate.primaryGen.BooleanTypeRep
                      override val dataToCondition = { cols: Seq[Any] =>
                        val col = cols(index).asInstanceOf[oneToOneUpdate.TargetType]
                        val slickData = oneToOneUpdate.filterConvert(data.get)
                        oneToOneUpdate.primaryGen.dataToCondition(col)(slickData)
                      }
                      override val wt = oneToOneUpdate.primaryGen.wt
                    }
                  } :: Nil
                  override val updateIndices = source.updateIndices
                  override val updateShapes = source.updateShapes
                  override val updateData = source.updateData
                }
              }
            }
          }

          val uSlickWriter = USWriter2(
            mainCol = slickWriter.mainCol,
            mainShape = slickWriter.mainShape,
            table = slickWriter.owner,
            data = slickWriter.convert(data.get),
            primaryGen = slickWriter.primaryGen.map { eachPri => (new FilterColumnGen[slickWriter.TargetType] {
              override type BooleanTypeRep = eachPri.BooleanTypeRep
              override val dataToCondition = { sourceCol: slickWriter.TargetType =>
                eachPri.dataToCondition(sourceCol)(
                  slickWriter.filterConvert(data.get)
                )
              }
              override val wt = eachPri.wt
            }) },
            subGen = uSlickSubGen
          )
          uSlickWriter: USlickWriter2
        } }
    } { genList =>
      { binds: List[(Any, SlickQueryBindImpl)] =>
        UpdateOperation2222.parseInsert(binds, genList)
      }
    }
  }


  def convert(columns: FColumn)(implicit ec: ExecutionContext): USlickWriter2 = {
    val slickWriter = FColumn.find(columns)({ case s: SlickUpdate[columns.DataType] => s })
    val oneToOneUpdateOpt = FColumn.findOpt(columns)({ case s: OneToOneUpdate[columns.DataType] => s })

    val uSlickSubGen = oneToOneUpdateOpt.map { oneToOneUpdate =>
      new UUpdateTran2 {
        override val table = oneToOneUpdate.owner
        override def convert(source: UpdateQuery): UpdateQuery = {
          new UpdateQuery {

            override val bind = source.bind
            override val cols = source.cols ::: oneToOneUpdate.mainCol :: Nil
            override val shapes = source.shapes ::: oneToOneUpdate.mainShape :: Nil
            override val filters = source.filters ::: {
              val index = cols.indexOf(oneToOneUpdate.mainCol)
              new FilterColumnGen[Seq[Any]] {
                override type BooleanTypeRep = oneToOneUpdate.primaryGen.BooleanTypeRep
                override val dataToCondition = { cols: Seq[Any] =>
                  val col = cols(index).asInstanceOf[oneToOneUpdate.TargetType]
                  val slickData = oneToOneUpdate.filterConvert(columns.data.get)
                  oneToOneUpdate.primaryGen.dataToCondition(col)(slickData)
                }
                override val wt = oneToOneUpdate.primaryGen.wt
              }
            } :: Nil
            override val updateIndices = source.updateIndices
            override val updateShapes = source.updateShapes
            override val updateData = source.updateData
          }
        }
      }
    }

    val uSlickWriter = USWriter2(
      mainCol = slickWriter.mainCol,
      mainShape = slickWriter.mainShape,
      table = slickWriter.owner,
      data = slickWriter.convert(columns.data.get),
      primaryGen = slickWriter.primaryGen.map { eachPri => (new FilterColumnGen[slickWriter.TargetType] {
        override type BooleanTypeRep = eachPri.BooleanTypeRep
        override val dataToCondition = { sourceCol: slickWriter.TargetType =>
          eachPri.dataToCondition(sourceCol)(
            slickWriter.filterConvert(columns.data.get)
          )
        }
        override val wt = eachPri.wt
      }) },
      subGen = uSlickSubGen
    )
    uSlickWriter
  }

}

object UpdateOperation2222 {

  def parseInsertGen(
                      binds: List[(Any, SlickQueryBindImpl)],
                      //updateList: List[FColumn],
                      wrapList: List[USlickWriter2],
                      converts: List[UUpdateTran2]
                    )(
                      implicit
                      ec: ExecutionContext,
                      updateConV: Query[_, Seq[Any], Seq] => JdbcActionComponent#UpdateActionExtensionMethods[Seq[Any]]
                    ): DBIO[UpdateStaticManyInfo] = {
    //val wrapList = updateList.map(InUpdateConvert2.convert)

    val currents = wrapList.groupBy(_.table).filter { case (key, s) => converts.exists(t => key == t.table) }
    val results = currents.map { case (table, eachWrap) =>
      val initUpdateQuery: UpdateQuery = new UpdateQuery {
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
        override val updateIndices = eachWrap.toStream.zipWithIndex.filter(_._1.primaryGen.isEmpty).map(_._2).toList
        override val updateShapes = eachWrap.toStream.filter(_.primaryGen.isEmpty).map(_.mainShape.packedShape).toList
        override val updateData = eachWrap.toStream.filter(_.primaryGen.isEmpty).map(_.data).toList
      }
      val convertRetrieveQuery = converts.filter(_.table == table).foldLeft(initUpdateQuery) { (x, y) =>
        y.convert(x)
      }
      val query = Query(convertRetrieveQuery.cols)(new ListAnyShape[FlatShapeLevel](convertRetrieveQuery.shapes))
      val bindQuery = convertRetrieveQuery.bind.bind(query)
      val filterQuery = convertRetrieveQuery.filters.foldLeft(bindQuery) { (x, y) =>
        x.filter(s => y.dataToCondition(s))(y.wt)
      }
      val updateDBIO = filterQuery.map { cols => convertRetrieveQuery.updateIndices.map(index => cols(index)) }(new ListAnyShape[FlatShapeLevel](initUpdateQuery.updateShapes))
        .update(initUpdateQuery.updateData)
      for {
        effectRows <- updateDBIO
        subs = eachWrap.map(_.subGen.toList).flatten
        subResult <- parseInsertGen(binds, wrapList, subs)
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
                   binds: List[(Any, SlickQueryBindImpl)],
                   wrapList: List[USlickWriter2]
                 )(
                   implicit
                   ec: ExecutionContext,
                   updateConV: Query[_, Seq[Any], Seq] => JdbcActionComponent#UpdateActionExtensionMethods[Seq[Any]]
                 ): DBIO[UpdateStaticManyInfo] = {
    //val wrapList = updateList.map(InUpdateConvert2.convert)
    val subGensTables = wrapList.flatMap { t => t.subGen.toList.map(_.table) }
    val currents = wrapList.groupBy(_.table).filter { case (key, s) => subGensTables.forall(t => key != t) }
    val results = currents.map { case (table, eachWrap) =>
      val initUpdateQuery: UpdateQuery = new UpdateQuery {
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
        override val updateIndices = eachWrap.toStream.zipWithIndex.filter(_._1.primaryGen.isEmpty).map(_._2).toList
        override val updateShapes = eachWrap.toStream.filter(_.primaryGen.isEmpty).map(_.mainShape.packedShape).toList
        override val updateData = eachWrap.toStream.filter(_.primaryGen.isEmpty).map(_.data).toList
      }
      val convertRetrieveQuery = initUpdateQuery
      val query = Query(convertRetrieveQuery.cols)(new ListAnyShape[FlatShapeLevel](convertRetrieveQuery.shapes))
      val bindQuery = convertRetrieveQuery.bind.bind(query)
      val filterQuery = convertRetrieveQuery.filters.foldLeft(bindQuery) { (x, y) =>
        x.filter(s => y.dataToCondition(s))(y.wt)
      }
      val updateDBIO = filterQuery.map { cols => convertRetrieveQuery.updateIndices.map(index => cols(index)) }(new ListAnyShape[FlatShapeLevel](initUpdateQuery.updateShapes))
        .update(initUpdateQuery.updateData)
      for {
        effectRows <- updateDBIO
        subs = eachWrap.map(_.subGen.toList).flatten
        subResult <- parseInsertGen(binds, wrapList, subs)
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
/*object UpdateOperation {

  def parseInsertGen(
    binds: List[(Any, SlickQueryBindImpl)],
    updateList: List[FColumn],
    converts: List[UUpdateTran2]
  )(
    implicit
    ec: ExecutionContext,
    updateConV: Query[_, Seq[Any], Seq] => JdbcActionComponent#UpdateActionExtensionMethods[Seq[Any]]
  ): DBIO[UpdateStaticManyInfo] = {
    val wrapList = updateList.map(InUpdateConvert2.convert)

    val currents = wrapList.groupBy(_.table).filter { case (key, s) => converts.exists(t => key == t.table) }
    val results = currents.map { case (table, eachWrap) =>
      val initUpdateQuery: UpdateQuery = new UpdateQuery {
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
        override val updateIndices = eachWrap.toStream.zipWithIndex.filter(_._1.primaryGen.isEmpty).map(_._2).toList
        override val updateShapes = eachWrap.toStream.filter(_.primaryGen.isEmpty).map(_.mainShape.packedShape).toList
        override val updateData = eachWrap.toStream.filter(_.primaryGen.isEmpty).map(_.data).toList
      }
      val convertRetrieveQuery = converts.filter(_.table == table).foldLeft(initUpdateQuery) { (x, y) =>
        y.convert(x)
      }
      val query = Query(convertRetrieveQuery.cols)(new ListAnyShape[FlatShapeLevel](convertRetrieveQuery.shapes))
      val bindQuery = convertRetrieveQuery.bind.bind(query)
      val filterQuery = convertRetrieveQuery.filters.foldLeft(bindQuery) { (x, y) =>
        x.filter(s => y.dataToCondition(s))(y.wt)
      }
      val updateDBIO = filterQuery.map { cols => convertRetrieveQuery.updateIndices.map(index => cols(index)) }(new ListAnyShape[FlatShapeLevel](initUpdateQuery.updateShapes))
        .update(initUpdateQuery.updateData)
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
    binds: List[(Any, SlickQueryBindImpl)],
    updateList: List[FColumn]
  )(
    implicit
    ec: ExecutionContext,
    updateConV: Query[_, Seq[Any], Seq] => JdbcActionComponent#UpdateActionExtensionMethods[Seq[Any]]
  ): DBIO[UpdateStaticManyInfo] = {
    val wrapList = updateList.map(InUpdateConvert2.convert)

    val subGensTables = wrapList.flatMap { t => t.subGen.toList.map(_.table) }
    val currents = wrapList.groupBy(_.table).filter { case (key, s) => subGensTables.forall(t => key != t) }
    val results = currents.map { case (table, eachWrap) =>
      val initUpdateQuery: UpdateQuery = new UpdateQuery {
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
        override val updateIndices = eachWrap.toStream.zipWithIndex.filter(_._1.primaryGen.isEmpty).map(_._2).toList
        override val updateShapes = eachWrap.toStream.filter(_.primaryGen.isEmpty).map(_.mainShape.packedShape).toList
        override val updateData = eachWrap.toStream.filter(_.primaryGen.isEmpty).map(_.data).toList
      }
      val convertRetrieveQuery = initUpdateQuery
      val query = Query(convertRetrieveQuery.cols)(new ListAnyShape[FlatShapeLevel](convertRetrieveQuery.shapes))
      val bindQuery = convertRetrieveQuery.bind.bind(query)
      val filterQuery = convertRetrieveQuery.filters.foldLeft(bindQuery) { (x, y) =>
        x.filter(s => y.dataToCondition(s))(y.wt)
      }
      val updateDBIO = filterQuery.map { cols => convertRetrieveQuery.updateIndices.map(index => cols(index)) }(new ListAnyShape[FlatShapeLevel](initUpdateQuery.updateShapes))
        .update(initUpdateQuery.updateData)
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
}*/