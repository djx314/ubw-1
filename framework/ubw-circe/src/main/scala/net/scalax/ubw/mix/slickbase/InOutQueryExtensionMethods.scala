package net.scalax.fsn.mix.slickbase

import net.scalax.fsn.core.Pile
import net.scalax.fsn.mix.operation.InAndOutOperation
import net.scalax.fsn.slick.helpers.SlickQueryBindImpl
import net.scalax.fsn.slick.model.SlickParam
import net.scalax.fsn.slick.operation.ExecInfo3
import slick.ast.{ AnonSymbol, Ref }
import slick.dbio.DBIO
import slick.jdbc.{ JdbcBackend, JdbcProfile }
import slick.lifted._

import scala.concurrent.{ ExecutionContext, Future }

case class InOutQueryWrap(
    columns: List[Pile],
    crudBinds: List[(Any, SlickQueryBindImpl)],
    listQueryBind: SlickQueryBindImpl
) { self =>
  /*def result(
    slickParam: SlickParam,
    sourceDB: JdbcBackend#Database,
    targetDB: JdbcBackend#Database,
    groupedSize: Int
  )(
    implicit
    slickProfile: JdbcProfile,
    ec: ExecutionContext
  ): Future[List[ExecInfo3]] = {
    val execPlan = InAndOutOperation.json2SlickCreateOperation(self)
    val resultAction = execPlan(slickParam)
    sourceDB.run(resultAction).flatMap { futures =>
      futures.grouped(groupedSize).foldLeft(Future successful List.empty[ExecInfo3]) { (effectRow, futureList) =>
        lazy val insertActions = Future.sequence(futureList.map(_.apply))
        lazy val insertFuture = insertActions.flatMap(actions => targetDB.run(
          DBIO.sequence(actions.collect { case Some(s) => s })
        ))
        effectRow.flatMap { row =>
          insertFuture.map { insetResult =>
            row ::: insetResult
          }
        }
      }
    }
  }

  def updateResult(
    slickParam: SlickParam,
    sourceDB: JdbcBackend#Database,
    targetDB: JdbcBackend#Database,
    groupedSize: Int
  )(
    implicit
    slickProfile: JdbcProfile,
    ec: ExecutionContext
  ): Future[List[ExecInfo3]] = {
    val execPlan = InAndOutOperation.json2SlickUpdateOperation(self)
    val resultAction = execPlan(slickParam)
    sourceDB.run(resultAction).flatMap { futures =>
      futures.grouped(groupedSize).foldLeft(Future successful List.empty[ExecInfo3]) { (effectRow, futureList) =>
        lazy val insertActions = Future.sequence(futureList.map(_.apply))
        lazy val insertFuture = insertActions.flatMap { actions =>
          Future.sequence(actions.collect { case Some(s) => s }).map { s =>
            targetDB.run(DBIO.sequence(s))
          }.flatMap(identity)
        }
        effectRow.flatMap { row =>
          insertFuture.map { insetResult =>
            row ::: insetResult
          }
        }
      }
    }
  }*/
}

class InOutQueryExtensionMethods[E, U](val queryToExt: Query[E, U, Seq]) {

  def flatMap[A, B](f: E => InOutQueryWrap): InOutQueryWrap = {
    val generator = new AnonSymbol
    val aliased = queryToExt.shaped.encodeRef(Ref(generator)).value
    val fv = f(aliased)
    val slickJsonQuery = new SlickQueryBindImpl {
      override def bind[E, U](query: Query[E, U, Seq]): Query[E, U, Seq] = {
        val newQuery = fv.listQueryBind.bind(query)
        new WrappingQuery[E, U, Seq](new slick.ast.Bind(generator, queryToExt.toNode, newQuery.toNode), newQuery.shaped)
      }
    }
    InOutQueryWrap(fv.columns, fv.crudBinds, slickJsonQuery) //(fv.ec)
  }

  def map(ev: E => FQueryWrap)(implicit ec: ExecutionContext): InOutQueryWrap = {
    val generator = new AnonSymbol
    val aliased = queryToExt.shaped.encodeRef(Ref(generator)).value
    val columns = ev(aliased)
    val slickJsonQuery = new SlickQueryBindImpl {
      override def bind[E, U](query: Query[E, U, Seq]): Query[E, U, Seq] = {
        new WrappingQuery[E, U, Seq](new slick.ast.Bind(generator, queryToExt.toNode, query.toNode), query.shaped)
      }
    }
    InOutQueryWrap(columns.columns, columns.binds, slickJsonQuery) //(ec)
  }

  def filter[T <: Rep[_]: CanBeQueryCondition](f: E => T): PileListQueryExtensionMethods[E, U] = {
    val cv = implicitly[CanBeQueryCondition[T]]
    new PileListQueryExtensionMethods(queryToExt.filter(f)(cv))
  }

  def withFilter[T: CanBeQueryCondition](f: E => T): PileListQueryExtensionMethods[E, U] = {
    val cv = implicitly[CanBeQueryCondition[T]]
    new PileListQueryExtensionMethods(queryToExt.withFilter(f)(cv))
  }

  def filterNot[T <: Rep[_]: CanBeQueryCondition](f: E => T): PileListQueryExtensionMethods[E, U] = {
    val cv = implicitly[CanBeQueryCondition[T]]
    new PileListQueryExtensionMethods(queryToExt.filterNot(f)(cv))
  }

  def groupBy[K, T, G, P](f: E => K)(implicit kshape: Shape[_ <: FlatShapeLevel, K, T, G], vshape: Shape[_ <: FlatShapeLevel, E, _, P]): PileListQueryExtensionMethods[(G, Query[P, U, Seq]), (T, Query[P, U, Seq])] = {
    val newQuery = queryToExt.groupBy(f)(kshape, vshape)
    new PileListQueryExtensionMethods(newQuery)
  }

}