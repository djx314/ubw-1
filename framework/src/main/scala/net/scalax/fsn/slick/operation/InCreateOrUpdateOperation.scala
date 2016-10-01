package net.scalax.fsn.slick.operation

import indicator.rw.utils.SlickQueryBindImpl
import net.scalax.fsn.core.FColumn
import slick.relational.RelationalProfile

import scala.language.existentials
import scala.concurrent.ExecutionContext
import scala.util.Success

object InCreateOrUpdateOperation {

  import slick.jdbc.PostgresProfile.api._

  def parseInsert(
    insertQuerytWrap: List[(RelationalProfile#Table[_], SlickQueryBindImpl)],
    columns: List[FColumn]
  )(
    implicit
    ec: ExecutionContext
  ): DBIO[ExecInfo] = {
    //println("3333" * 100)
    InRetrieveOperation.parseInsert(insertQuerytWrap, columns).asTry.flatMap {
      case Success(_) =>
        //println("1111" * 100)
        InUpdateOperation.parseInsert(insertQuerytWrap, columns).map { s =>
          ExecInfo(s.effectRows, Nil)
        }
      case s =>
        //println(s.isSuccess)
        //println("2222" * 100)
        InCreateOperation.parseInsert(insertQuerytWrap, columns).map { s =>
          ExecInfo(s.effectRows, Nil)
        }
    }
    /*asTry.flatMap {
      case Success(_) =>
        println("1111" * 100)
        UpdateWrapDeal.parseInsert(insertQuerytWrap, columns).map { s =>
          ExecInfo(s.effectRows, Nil)
        }
      case s =>
        println(s.isSuccess)
        println("2222" * 100)
        InsertWrapDeal.parseInsert(insertQuerytWrap, columns)
    }*/

  }
}