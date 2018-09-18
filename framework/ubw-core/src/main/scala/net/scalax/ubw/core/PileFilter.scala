package net.scalax.ubw.core

import cats.{ Functor, Monad, Semigroup, Traverse }
import cats.instances.list._
import cats._

import net.scalax.ubw.core._

import scala.language.higherKinds

trait PileFilter[E] {

  type TempDataType
  val filter: AtomicPath => QueryTranform[TempDataType]
  val listGen: List[TempDataType] => E

  def transform(dataPiles: List[DataPile]): E = {
    val tempList = dataPiles.flatMap { dataPile =>
      dataPile.pathWithValues.flatMap { pAndV =>
        val wrap = filter(pAndV.path)
        wrap.gen match {
          case Left(_) => Option.empty[TempDataType]
          case Right(tran) =>
            Option(wrap.apply(tran, pAndV.value.asInstanceOf[AtomicValueImpl[wrap.path.DataType]]))
        }
      }
    }
    listGen(tempList)
  }

}

object PileFilter {

  def apply[U, E](pathGen: AtomicPath => QueryTranform[U])(columnGen: List[U] => E): PileFilter[E] = {
    new PileFilter[E] {
      override type TempDataType = U
      override val filter = pathGen
      override val listGen = columnGen
    }
  }

}