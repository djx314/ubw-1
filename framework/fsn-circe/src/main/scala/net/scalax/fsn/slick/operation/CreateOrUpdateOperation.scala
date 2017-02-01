package net.scalax.fsn.slick.operation

import net.scalax.fsn.core.FColumn
import net.scalax.fsn.slick.helpers.SlickQueryBindImpl
import slick.relational.RelationalProfile

import scala.language.existentials
import scala.concurrent.ExecutionContext
import scala.util.Success

object CreateOrUpdateOperation {

  import slick.jdbc.PostgresProfile.api._

  def parseInsert(
    insertQuerytWrap: List[(Any, SlickQueryBindImpl)],
    columns: List[FColumn]
  )(
    implicit
    ec: ExecutionContext
  ): DBIO[ExecInfo] = {
    RetrieveOperation.parseInsert(insertQuerytWrap, columns).asTry.flatMap {
      case Success(_) =>
        /*UpdateOperation.parseInsert(insertQuerytWrap, columns).map { s =>
          ExecInfo(s.effectRows, Nil)
        }*/
        ???
      case s =>
        /*CreateOperation.parseInsert(insertQuerytWrap, columns).map { s =>
          ExecInfo(s.effectRows, Nil)
        }*/
        ???
    }
  }
}