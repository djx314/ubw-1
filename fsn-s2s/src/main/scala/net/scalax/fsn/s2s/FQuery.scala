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
    SlickQuery(fv.sourceQuery)(query2)(fv.transafrom)
  }

  def map(f: E => List[SlickConverter]): SlickQuery = {
    flatMap(s => {
      val wrapper = f(s).reduce(_ append _)
      val query1: Query[wrapper.writer.TargetColumn, wrapper.writer.DataType, Seq] = Query(wrapper.writer.sourceColumn)(wrapper.writer.writer)
      val query2: Query[wrapper.reader.TargetColumn, wrapper.reader.DataType, Seq] = Query(wrapper.reader.sourceColumn)(wrapper.reader.reader)
      SlickQuery(query2)(query1)(wrapper.convert)
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
    SlickQuery(query2)(fv.targetQuery)(fv.transafrom)
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

object SlickQuery {

  def apply[SE1, SU1, TE1, TU1](sourceQuery1: Query[SE1, SU1, Seq])(targetQuery1: Query[TE1, TU1, Seq])(transafrom1: SU1 => TU1) = {
    new SlickQuery {
      override type SE = SE1
      override type SU = SU1
      override type TE = TE1
      override type TU = TU1
      override val sourceQuery = sourceQuery1
      override val targetQuery = targetQuery1
      override val transafrom = transafrom1
    }
  }

}