package net.scalax.fsn.s2s

import slick.ast.{AnonSymbol, Bind, Ref}
import slick.driver.JdbcActionComponent
import slick.jdbc.JdbcBackend
import slick.lifted._
import scala.concurrent.{ExecutionContext, Future}

class TargetQueryExtensionMethods[E, U](val queryToExt: Query[E, U, Seq]) {

  def flatMap(f: E => SlickQuery): SlickQuery = {
    val generator = new AnonSymbol
    val aliased = queryToExt.shaped.encodeRef(Ref(generator)).value
    val fv = f(aliased)
    val query2 = new WrappingQuery[fv.TE, fv.TU, Seq](new Bind(generator, queryToExt.toNode, fv.targetQuery.toNode), fv.targetQuery.shaped)
    new SlickQuery {
      override type SE = fv.SE
      override type SU = fv.SU
      override val sourceQuery = fv.sourceQuery

      override type TE = fv.TE
      override type TU = fv.TU
      override val targetQuery = query2

      override val transafrom = fv.transafrom
    }
  }

  def map(f: E => List[SlickWrapper]): SlickQuery = {
    flatMap(s => {
      val wrapper = f(s).reduce(_ append _)
      val query1: Query[wrapper.writer.TargetColumn, wrapper.writer.DataType, Seq] = Query(wrapper.writer.sourceColumn)(wrapper.writer.writer)
      val query2: Query[wrapper.reader.TargetColumn, wrapper.reader.DataType, Seq] = Query(wrapper.reader.sourceColumn)(wrapper.reader.reader)
      new SlickQuery {
        override type SE = wrapper.reader.TargetColumn
        override type SU = wrapper.reader.DataType
        override val sourceQuery = query2

        override type TE = wrapper.writer.TargetColumn
        override type TU = wrapper.writer.DataType
        override val targetQuery = query1

        override val transafrom = wrapper.convert
      }
    })
  }

}

class SourceQueryExtensionMethods[E, U](val queryToExt: Query[E, U, Seq]) {

  def flatMap(f: E => SlickQuery): SlickQuery = {
    val generator = new AnonSymbol
    val aliased = queryToExt.shaped.encodeRef(Ref(generator)).value
    val fv = f(aliased)
    val fvQuery = fv.sourceQuery
    val query2 = new WrappingQuery[fv.SE, fv.SU, Seq](new Bind(generator, queryToExt.toNode, fvQuery.toNode), fvQuery.shaped)
    new SlickQuery {
      override type SE = fv.SE
      override type SU = fv.SU
      override val sourceQuery = query2

      override type TE = fv.TE
      override type TU = fv.TU
      override val targetQuery = fv.targetQuery

      override val transafrom = fv.transafrom
    }
  }

}

trait SlickQuery {

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