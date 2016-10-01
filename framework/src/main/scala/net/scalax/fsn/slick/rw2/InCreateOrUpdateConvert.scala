package indicator.rw.utils.rw2

import indicator.rw.utils.SlickQueryBindImpl
import net.scalax.fsn.core.FColumn
import slick.basic.BasicProfile
import slick.relational.RelationalProfile

import scala.language.existentials
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

object InCreateOrUpdateConvert {

  import slick.jdbc.PostgresProfile.api._

  def parseInsert(
    insertQuerytWrap: List[(RelationalProfile#Table[_], SlickQueryBindImpl)],
    columns: List[FColumn]
  )(
    implicit
    ec: ExecutionContext
  ): DBIO[ExecInfo] = {
    //println("3333" * 100)
    RetrieveWrapDeal2.parseInsert(insertQuerytWrap, columns).asTry.flatMap {
      case Success(_) =>
        //println("1111" * 100)
        UpdateWrapDeal2.parseInsert(insertQuerytWrap, columns).map { s =>
          ExecInfo(s.effectRows, Nil)
        }
      case s =>
        //println(s.isSuccess)
        //println("2222" * 100)
        InsertWrapDeal2.parseInsert(insertQuerytWrap, columns).map { s =>
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