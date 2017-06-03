package net.scalax.fsn.mix.operation

import net.scalax.fsn.core._
import net.scalax.fsn.slick.atomic._
import net.scalax.fsn.json.operation._
import net.scalax.fsn.mix.slickbase.InOutQueryWrap
import net.scalax.fsn.slick.model.SlickParam
import net.scalax.fsn.slick.operation.{ ExecInfo3, InCreateConvert, StrOutSelectConvert }
import slick.dbio.{ DBIO, NoStream }
import slick.jdbc.JdbcActionComponent
import slick.lifted.{ Query, Rep }

import scala.concurrent.Future
import scala.concurrent.ExecutionContext

object InAndOutOperation extends FPilesGenHelper with FAtomicValueHelper {

  def futureGen(implicit ec: ExecutionContext): FPileSyntax.PileGen[Future[Option[List[FAtomicValue]]]] = {
    FPile.transformTreeList {
      new FAtomicQuery(_) {
        val aa = withRep(needAtomic[SlickCreate])
          .mapTo {
            case (_, data) =>
              (data match {
                case dataWrap @ FSomeValue(_) =>
                  Future successful dataWrap
                case FFValue(futureData) =>
                  futureData.map(set)
                case FAtomicValueImpl.Zero => Future successful FAtomicValueImpl.Zero
                case x =>
                  println(x.atomics)
                  Future successful x
              }): Future[FAtomicValue]
          }
      }.aa
    } { genList =>
      Future.sequence(genList).map(Option(_)).recover {
        case e: NoSuchElementException =>
          None
        case e: Exception =>
          e.printStackTrace
          None
      }
    }
  }

  def json2SlickCreateOperation(binds: InOutQueryWrap)(
    implicit
    ec: ExecutionContext,
    jsonEv: Query[_, List[Any], List] => JdbcActionComponent#StreamingQueryActionExtensionMethods[List[List[Any]], List[Any]],
    repToDBIO: Rep[Int] => JdbcActionComponent#QueryActionExtensionMethods[Int, NoStream],
    cv: Query[_, Seq[Any], Seq] => JdbcActionComponent#InsertActionExtensionMethods[Seq[Any]],
    retrieveCv: Query[_, Seq[Any], Seq] => JdbcActionComponent#StreamingQueryActionExtensionMethods[Seq[Seq[Any]], Seq[Any]]
  ): SlickParam => DBIO[List[() => Future[Option[DBIO[ExecInfo3]]]]] = {
    { param: SlickParam =>
      val gen = StrOutSelectConvert.ubwGen(binds.listQueryBind).flatMap(futureGen) { (slickReader, futureConvert) =>
        slickReader.slickResult.apply(param).resultAction.map { action =>
          val data = action.data
          println("已获得全部数据")
          data.map(s => () => futureConvert(s))
        }
      }.flatMap(InCreateConvert.createGen) { (execAction, slickWriterGen) =>
        execAction.map { futureList =>
          futureList.map { eachFuture => () =>
            eachFuture().map {
              _.map {
                t =>
                  slickWriterGen(t)(binds.crudBinds)
              }
            }
          }
        }
      }

      gen.result(binds.columns) match {
        case Left(e: Exception) =>
          e.printStackTrace()
          throw e
        case Right(s) => s
      }
    }

  }

}