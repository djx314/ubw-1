package net.scalax.fsn.mix.operation

import net.scalax.fsn.core._
import net.scalax.fsn.slick.atomic._
import net.scalax.fsn.json.operation._
//import net.scalax.fsn.mix.slickbase.InOutQueryWrap
//import net.scalax.fsn.slick.model.SlickParam
//import net.scalax.fsn.slick.operation.{ ExecInfo3, InCreateConvert, InUpdateConvert }
//import slick.jdbc.JdbcProfile

import scala.concurrent.Future
import scala.concurrent.ExecutionContext

object InAndOutOperation extends PilesGenHelper with AtomicValueHelper {

  def futureGen(implicit ec: ExecutionContext): InputChannel[Future[Option[DataPileContent]]] = {
    DataPile.transformTree {
      new AtomicQuery(_) {
        val aa = withRep(needAtomic[SlickCreate])
          .mapTo {
            case (_, data) =>
              (data match {
                case dataWrap @ FSomeValue(_) =>
                  Future successful dataWrap
                case FFValue(futureData) =>
                  futureData.map(set)
                case zero @ AtomicValueImpl.Zero() => Future successful zero
                case x =>
                  println(x.atomics)
                  Future successful x
              }): Future[AtomicValue]
          }
      }.aa
    } { (genList, atomicValueGen) =>
      Future.sequence(genList).map(s => Option(atomicValueGen.toContent(s))).recover {
        case _: NoSuchElementException =>
          //忽略因错误在数据库取不到数据的行
          None
        case e: Exception =>
          e.printStackTrace
          None
      }
    }
  }

  //TODO 底层恢复使用后恢复此函数
  /*def json2SlickCreateOperation(binds: InOutQueryWrap)(
    implicit
    slickProfile: JdbcProfile,
    ec: ExecutionContext
  ): SlickParam => slickProfile.api.DBIO[List[() => Future[Option[slickProfile.api.DBIO[ExecInfo3[List[DataWithIndex]]]]]]] = {
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

  def json2SlickUpdateOperation(binds: InOutQueryWrap)(
    implicit
    slickProfile: JdbcProfile,
    ec: ExecutionContext
  ): SlickParam => slickProfile.api.DBIO[List[() => Future[Option[Future[slickProfile.api.DBIO[ExecInfo3[List[DataWithIndex]]]]]]]] = {
    { param: SlickParam =>
      val gen = StrOutSelectConvert.ubwGen(binds.listQueryBind).flatMap(futureGen) { (slickReader, futureConvert) =>
        slickReader.slickResult.apply(param).resultAction.map { action =>
          val data = action.data
          println("已获得全部数据")
          data.map(s => () => futureConvert(s))
        }
      }.flatMap(InUpdateConvert.updateGen) { (execAction, slickWriterGen) =>
        execAction.map { futureList =>
          futureList.map { eachFuture => () =>
            eachFuture().map {
              _.map {
                t =>
                  //slickWriterGen
                  slickWriterGen(t)._1.apply(binds.crudBinds)
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
  }*/

}