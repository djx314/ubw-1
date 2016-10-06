package net.scalax.fsn.slick.operation

import net.scalax.fsn.core.FColumn
import net.scalax.fsn.slick.atomic.{AutoInc, OneToOneCrate, SlickCreate}
import net.scalax.fsn.slick.helpers.{ListAnyShape, SlickQueryBindImpl, SlickUtils}
import slick.basic.BasicProfile
import slick.dbio.DBIO
import slick.jdbc.JdbcActionComponent
import slick.lifted.{FlatShapeLevel, Query, Shape}
import slick.relational.RelationalProfile

import scala.concurrent.ExecutionContext
import scala.language.existentials

case class ExecInfo(effectRows: Int, columns: List[ColumnWithIndex]) {

  def fColumns = columns.map(_.column)

}

case class ColumnWithIndex(column: FColumn, index: Int)

trait InsertWrapTran2[I] {
  val table: Any
  def convert(inc: I, source: InsertDataQuery): InsertDataQuery
}

trait InsertDataQuery {

  val bind: SlickQueryBindImpl
  val cols: List[Any]
  val shapes: List[Shape[_ <: FlatShapeLevel, _, _, _]]
  val data: List[Any]
  val returningCols: List[Any]
  val returningShapes: List[Shape[_ <: FlatShapeLevel, _, _, _]]

}

trait ISlickWriter2 {
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
  val subGen: Option[InsertWrapTran2[IncValue]]
}

case class ISWriter2[A, B, C, D, E, F](
  override val preData: B,
  override val table: Any,
  override val preRep: A,
  override val preShape: Shape[_ <: FlatShapeLevel, A, B, C],
  override val autoIncRep: D,
  override val autoIncShape: Shape[_ <: FlatShapeLevel, D, E, F],
  override val subGen: Option[InsertWrapTran2[E]]
) extends ISlickWriter2 {
  override type PreRep = A
  override type PreValue = B
  override type PreTarget = C
  override type IncRep = D
  override type IncValue = E
  override type IncTarget = F
}

object InCreateConvert2 {
  def convert(column: FColumn)(implicit ec: ExecutionContext): ISlickWriter2 = {
    val slickCreate = FColumn.find(column) { case s: SlickCreate[column.DataType] => s }
    val isAutoInc = FColumn.findOpt(column) { case s : AutoInc[column.DataType] => s }.map(_.isAutoInc).getOrElse(false)
    val oneToOneCreateOpt = FColumn.findOpt(column) { case s: OneToOneCrate[column.DataType] => s }

    if (isAutoInc) {
      lazy val oneToOneSubGen = oneToOneCreateOpt.map { oneToOneCreate => new InsertWrapTran2[slickCreate.SlickType] {
        override val table = oneToOneCreate.owner
        def convert(sourceData: slickCreate.SlickType, source: InsertDataQuery): InsertDataQuery = {
          new InsertDataQuery {
            override val bind = source.bind
            override val cols = source.cols ::: oneToOneCreate.mainCol :: Nil
            override val shapes = source.shapes ::: oneToOneCreate.mainShape :: Nil
            override val data = source.data ::: oneToOneCreate.convert(slickCreate.convert(sourceData)) :: Nil
            override val returningCols = source.returningCols
            override val returningShapes = source.returningShapes
          }
        }
      } }

      ISWriter2(
        preData = (),
        table = slickCreate.owner,
        preRep = (),
        preShape = implicitly[Shape[FlatShapeLevel, Unit, Unit, Unit]],
        autoIncRep = (slickCreate.mainCol: slickCreate.SourceType),
        autoIncShape = slickCreate.mainShape,
        subGen = oneToOneSubGen
      )
    } else {
      lazy val oneToOneSubGen = oneToOneCreateOpt.map { oneToOneCreate => new InsertWrapTran2[Unit] {
        override val table = oneToOneCreate.owner
        def convert(sourceData: Unit, source: InsertDataQuery): InsertDataQuery = {
          new InsertDataQuery {
            override val bind = source.bind
            override val cols = source.cols ::: oneToOneCreate.mainCol :: Nil
            override val shapes = source.shapes ::: oneToOneCreate.mainShape :: Nil
            override val data = source.data ::: oneToOneCreate.convert(column.data.get) :: Nil
            override val returningCols = source.returningCols
            override val returningShapes = source.returningShapes
          }
        }
      } }

      ISWriter2(
        preData = {
          slickCreate.reverseConvert(column.data.get)
        },
        table = slickCreate.owner,
        preRep = (slickCreate.mainCol: slickCreate.SourceType),
        preShape = slickCreate.mainShape,
        autoIncRep = (),
        autoIncShape = implicitly[Shape[FlatShapeLevel, Unit, Unit, Unit]],
        subGen = oneToOneSubGen
      )
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
    insertList: List[FColumn],
    converts: List[InsWrapTran2]
  )(
    implicit
    ec: ExecutionContext,
    cv: Query[_, Seq[Any], Seq] => JdbcActionComponent#InsertActionExtensionMethods[Seq[Any]],
    retrieveCv: Query[_, Seq[Any], Seq] => BasicProfile#StreamingQueryActionExtensionMethods[Seq[Seq[Any]], Seq[Any]]
  ): DBIO[ExecInfo2] = try {
    val wrapList = insertList.map(InCreateConvert2.convert)

    val currents = wrapList.groupBy(_.table).filter { case (key, s) => converts.exists(t => key == t.table) }
    val results = currents.map { case (table, eachWrap) =>
      val initCreateQuery: InsertDataQuery = new InsertDataQuery {
        override val bind = binds.find(_._1 == table).get._2
        override val cols = eachWrap.map(_.preRep)
        override val shapes = eachWrap.map(_.preShape)
        override val data = eachWrap.map(_.preData)
        override val returningCols = eachWrap.map(_.autoIncRep)
        override val returningShapes = eachWrap.map(_.autoIncShape)
      }
      val convertRetrieveQuery = converts.filter(_.table == table).foldLeft(initCreateQuery) { (x, y) =>
        y.convert(x)
      }
      val query = Query(convertRetrieveQuery.cols)(new ListAnyShape[FlatShapeLevel](convertRetrieveQuery.shapes))
      val bindQuery = convertRetrieveQuery.bind.bind(query)
      val returningShape = new ListAnyShape[FlatShapeLevel](convertRetrieveQuery.returningShapes)
      val returingQuery = Query(convertRetrieveQuery.returningCols)(returningShape)
      val incDataDBIO = if (SlickUtils.isShapeEmpty(returningShape)) {
        (bindQuery += convertRetrieveQuery.data) >> returingQuery.result
      } else {
        val bindReturingQuery = convertRetrieveQuery.bind.bind(returingQuery)
        val createQuery = bindQuery returning bindReturingQuery
        createQuery += convertRetrieveQuery.data
      }
      for {
        incData <- incDataDBIO
        fillSubGens = eachWrap.zip(incData).map { case (wrap, dataItem) =>
          val wrapSlickData = dataItem.asInstanceOf[wrap.IncValue]
          val subGens = wrap.subGen.map { gen => new InsWrapTran2 {
            override val table = gen.table
            override def convert(source: InsertDataQuery): InsertDataQuery = {
              gen.convert(wrapSlickData, source)
            }
          } }
          subGens
        }
        subResult <- parseInsertGen(binds, insertList, fillSubGens.map(_.toList).flatten)
      } yield {
        ExecInfo2(subResult.effectRows + 1, subResult.columns)
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
    insertList: List[FColumn]
  )(
    implicit
    ec: ExecutionContext,
    cv: Query[_, Seq[Any], Seq] => JdbcActionComponent#InsertActionExtensionMethods[Seq[Any]],
    retrieveCv: Query[_, Seq[Any], Seq] => BasicProfile#StreamingQueryActionExtensionMethods[Seq[Seq[Any]], Seq[Any]]
  ): DBIO[ExecInfo2] = try {
    val wrapList = insertList.map(InCreateConvert2.convert)

    val subGensTables = wrapList.flatMap { t => t.subGen.toList.map(_.table) }
    val currents = wrapList.groupBy(_.table).filter { case (key, s) => subGensTables.forall(t => key != t) }
    val results = currents.map { case (table, eachWrap) =>
      val initCreateQuery: InsertDataQuery = new InsertDataQuery {
        override val bind = binds.find(_._1 == table).get._2
        override val cols = eachWrap.map(_.preRep)
        override val shapes = eachWrap.map(_.preShape)
        override val data = eachWrap.map(_.preData)
        override val returningCols = eachWrap.map(_.autoIncRep)
        override val returningShapes = eachWrap.map(_.autoIncShape)
      }
      val convertRetrieveQuery = initCreateQuery
      val query = Query(convertRetrieveQuery.cols)(new ListAnyShape[FlatShapeLevel](convertRetrieveQuery.shapes))
      val bindQuery = convertRetrieveQuery.bind.bind(query)
      val returningShape = new ListAnyShape[FlatShapeLevel](convertRetrieveQuery.returningShapes)
      val returingQuery = Query(convertRetrieveQuery.returningCols)(returningShape)
      val incDataDBIO = if (SlickUtils.isShapeEmpty(returningShape)) {
        (bindQuery += convertRetrieveQuery.data) >> returingQuery.result
      } else {
        val bindReturingQuery = convertRetrieveQuery.bind.bind(returingQuery)
        val createQuery = bindQuery returning bindReturingQuery
        createQuery += convertRetrieveQuery.data
      }
      for {
        incData <- incDataDBIO
        fillSubGens = eachWrap.zip(incData).map { case (wrap, dataItem) =>
          val wrapSlickData = dataItem.asInstanceOf[wrap.IncValue]
          val subGens = wrap.subGen.map { gen => new InsWrapTran2 {
            override val table = gen.table
            override def convert(source: InsertDataQuery): InsertDataQuery = {
              gen.convert(wrapSlickData, source)
            }
          } }
          subGens
        }
        subResult <- parseInsertGen(binds, insertList, fillSubGens.map(_.toList).flatten)
      } yield {
        ExecInfo2(subResult.effectRows + 1, subResult.columns)
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

}