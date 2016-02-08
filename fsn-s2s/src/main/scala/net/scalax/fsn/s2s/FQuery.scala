package net.scalax.fsn.s2s

import slick.ast.{AnonSymbol, Bind, Ref}
import slick.driver.JdbcActionComponent
import slick.jdbc.JdbcBackend
import slick.lifted._

import scala.concurrent.{Future, ExecutionContext}
import scala.language.higherKinds

class TargetQueryExtensionMethods[E, U](val queryToExt: Query[E, U, Seq]) {

  trait TempWrapper {
    def query: Query[wrapper.writer.TargetColumn, wrapper.writer.DataType, Seq]
    val wrapper: SlickWrapper
  }

  def flatMap(f: E => FQuery)
  : FQuery = {
    val generator = new AnonSymbol
    val aliased = queryToExt.shaped.encodeRef(Ref(generator)).value
    val fv = f(aliased)
    val query2 = new WrappingQuery[fv.TE, fv.TU, Seq](new Bind(generator, queryToExt.toNode, fv.targetQuery.toNode), fv.targetQuery.shaped)
    new FQuery {
      override type TE = fv.TE
      override type TU = fv.TU
      override type SE = fv.SE
      override type SU = fv.SU
      override val sourceQuery = fv.sourceQuery
      override val targetQuery = query2
      override val transafrom = fv.transafrom
    }
  }

  def map(f: E => List[SlickWrapper]): FQuery = {
    flatMap(s => {
      val selectRep = f(s).reduce(_ append _)
      val query1: Query[selectRep.writer.TargetColumn, selectRep.writer.DataType, Seq] = Query(selectRep.writer.sourceColumn)(selectRep.writer.writer)
      val query2: Query[selectRep.reader.TargetColumn, selectRep.reader.DataType, Seq] = Query(selectRep.reader.sourceColumn)(selectRep.reader.reader)
      new FQuery {
        override type TE = selectRep.writer.TargetColumn
        override type TU = selectRep.writer.DataType
        override val targetQuery = query1
        override type SE = selectRep.reader.TargetColumn
        override type SU = selectRep.reader.DataType
        override val sourceQuery = query2
        override val transafrom = selectRep.convert
      }
    })
  }

}

class SourceQueryExtensionMethods[E, U](val queryToExt: Query[E, U, Seq]) {

  def flatMap(f: E => FQuery)
  : FQuery = {
    val generator = new AnonSymbol
    val aliased = queryToExt.shaped.encodeRef(Ref(generator)).value
    val fv = f(aliased)
    val fvQuery = fv.sourceQuery
    val query2 = new WrappingQuery[fv.SE, fv.SU, Seq](new Bind(generator, queryToExt.toNode, fvQuery.toNode), fvQuery.shaped)
    new FQuery {
      override type TE = fv.TE
      override type TU = fv.TU
      override type SE = fv.SE
      override type SU = fv.SU
      override val sourceQuery = query2
      override val targetQuery = fv.targetQuery
      override val transafrom = fv.transafrom
    }
  }

}

trait FQuery {

  type SE
  type SU
  val sourceQuery: Query[SE, SU, Seq]

  type TE
  type TU
  val targetQuery: Query[TE, TU, Seq]

  val transafrom: SU => TU

  def db2db(
    sourceDB: JdbcBackend#Database,
    targetDB: JdbcBackend#Database
  )(implicit
    ec: ExecutionContext,
    query2DBIO: Query[SE, SU, Seq] => JdbcActionComponent#StreamingQueryActionExtensionMethods[Seq[SU], SU],
    query2InsertDBIO: Query[TE, TU, Seq] => JdbcActionComponent#InsertActionExtensionMethods[TU]
  ): Future[Option[Int]] = {
    sourceDB.run(query2DBIO(sourceQuery).result).map(s => {
      val dataToInsert = s.map(t => transafrom(t))
      val insertDBIO = query2InsertDBIO(targetQuery) ++= dataToInsert
      targetDB.run(insertDBIO)
    }).flatMap(s => s)
  }

}

trait FConvert {

  implicit class columnExtendMethods[T, S, R](repLike: T)(implicit sShape: Shape[_ <: FlatShapeLevel, T, S, R]) {

    def setTo[H, B, U](targetRepLike: H)(implicit tShape: Shape[_ <: FlatShapeLevel, H, B, U]): (S => B) => SlickWrapper = {
      (convert1) => {
        val reader1 = new SlickReader {
          override type SourceColumn = T
          override type TargetColumn = R
          override val sourceColumn = repLike
          override type DataType = S
          override val reader = sShape
        }
        val writer1 = new SlickWriter {
          override type SourceColumn = H
          override type TargetColumn = U
          override val sourceColumn = targetRepLike
          override type DataType = B
          override val writer = tShape
        }
        new SlickWrapper {
          override val writer = writer1
          override val reader = reader1
          override val convert = convert1
        }
      }
    }

    def setToSame[H, U](targetRepLike: H)(implicit tShape: Shape[_ <: FlatShapeLevel, H, S, U]): SlickWrapper = {
      setTo(targetRepLike)(tShape)((s: S) => s)
    }

  }

}

trait SlickWrapper {

  val writer: SlickWriter

  val reader: SlickReader

  val convert: reader.DataType => writer.DataType

  def append(slickWrapper: SlickWrapper): SlickWrapper = {

    type RSourceColumn = (this.reader.SourceColumn, slickWrapper.reader.SourceColumn)
    type RTargetColumn = (this.reader.TargetColumn, slickWrapper.reader.TargetColumn)
    val rSourceColumn = this.reader.sourceColumn -> slickWrapper.reader.sourceColumn
    type RDataType = (this.reader.DataType, slickWrapper.reader.DataType)
    val rShape = new TupleShape[FlatShapeLevel, RSourceColumn, RDataType, RTargetColumn](this.reader.reader, slickWrapper.reader.reader)
    val newReader = new SlickReader {
      override type SourceColumn = RSourceColumn
      override type TargetColumn = RTargetColumn
      override type DataType = RDataType
      override val reader = rShape
      override val sourceColumn = rSourceColumn
    }

    type WSourceColumn = (this.writer.SourceColumn, slickWrapper.writer.SourceColumn)
    type WTargetColumn = (this.writer.TargetColumn, slickWrapper.writer.TargetColumn)
    val wSourceColumn = this.writer.sourceColumn -> slickWrapper.writer.sourceColumn
    type WDataType = (this.writer.DataType, slickWrapper.writer.DataType)
    val wShape = new TupleShape[FlatShapeLevel, WSourceColumn, WDataType, WTargetColumn](this.writer.writer, slickWrapper.writer.writer)
    val newWriter = new SlickWriter {
      override type SourceColumn = WSourceColumn
      override type TargetColumn = WTargetColumn
      override type DataType = WDataType
      override val writer = wShape
      override val sourceColumn = wSourceColumn
    }

    val newConvert: RDataType => WDataType = (rData) => {
      val f1 = this.convert(rData._1)
      val f2 = slickWrapper.convert(rData._2)
      f1 -> f2
    }

    new SlickWrapper {
      override val reader = newReader
      override val writer = newWriter
      override val convert = newConvert
    }
  }

}

trait SlickWriter extends FWriter {

  type SourceColumn
  type TargetColumn
  val sourceColumn: SourceColumn

  override type Writer[C] = Shape[_ <: FlatShapeLevel, SourceColumn, C, TargetColumn]

}

trait SlickReader extends FReader {

  type SourceColumn
  type TargetColumn
  val sourceColumn: SourceColumn

  override type Reader[C] = Shape[_ <: FlatShapeLevel, SourceColumn, C, TargetColumn]

}

trait FData {

  type DataType

  val data: DataType

}

trait FWriter {

  type DataType

  type Writer[_]

  val writer: Writer[DataType]

}

trait FReader {

  type DataType

  type Reader[_]

  val reader: Reader[DataType]

}