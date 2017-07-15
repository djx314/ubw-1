package net.scalax.fsn.core

import shapeless.{ ::, HList, HNil }

import scala.language.higherKinds
import scala.language.implicitConversions
import scala.reflect.runtime.universe._

trait AbstractFAtomicGen[U, S] {
  def getBy(atomics: List[FAtomic[U]]): Either[FAtomicException, S]
}

object AbstractFAtomicGen {
  def empty[U]: AbstractFAtomicGen[U, HNil] = new AbstractFAtomicGen[U, HNil] {
    def getBy(atomics: List[FAtomic[U]]): Either[FAtomicException, HNil] = Right(HNil)
  }

  class bbbb[T, F <: HList](bb: AbstractFAtomicGen[T, F]) {
    def ::[G](cc: AbstractFAtomicGen[T, G]): AbstractFAtomicGen[T, G :: F] = {
      new AbstractFAtomicGen[T, G :: F] {
        override def getBy(atomics: List[FAtomic[T]]): Either[FAtomicException, G :: F] = {
          (cc.getBy(atomics), bb.getBy(atomics)) match {
            case (Left(e1), Left(e2)) =>
              Left(FAtomicException(e1.typeTags ::: e2.typeTags))
            case (Left(e1), Right(_)) =>
              Left(FAtomicException(e1.typeTags))
            case (Right(_), Left(e2)) =>
              Left(FAtomicException(e2.typeTags))
            case (Right(out1), Right(out2)) =>
              Right(out1 :: out2)
          }
        }
      }
    }
  }

  implicit def convert[T, F <: HList](bb: AbstractFAtomicGen[T, F]): bbbb[T, F] = new bbbb(bb)
}

trait FAtomicGen[U, S[_]] extends AbstractFAtomicGen[U, S[U]] {
  override def getBy(atomics: List[FAtomic[U]]): Either[FAtomicException, S[U]]
}

trait FAtomicGenOpt[U, S[_]] extends AbstractFAtomicGen[U, Option[S[U]]] {
  override def getBy(atomics: List[FAtomic[U]]): Either[FAtomicException, Option[S[U]]]
}

trait FAtomicGenList[U, S[_]] extends AbstractFAtomicGen[U, List[S[U]]] {
  override def getBy(atomics: List[FAtomic[U]]): Either[FAtomicException, List[S[U]]]
}
/*trait FAtomicGenShape[-Input, U, S] {
  def unwrap(input: Input): AbstractFAtomicGen[U, S]
}

object FAtomicGenShape extends FAtomicGenShapeImpl {

}

trait FAtomicGenShapeImpl {
  implicit def hnilShape[U]: FAtomicGenShape[HNil, U, HNil] = new FAtomicGenShape[HNil, U, HNil] {
    override def unwrap(input: HNil): AbstractFAtomicGen[U, HNil] = new AbstractFAtomicGen[U, HNil] {
      override def getBy(atomics: List[FAtomic[U]]): Either[FAtomicException, HNil] = {
        Right(HNil)
      }
    }
  }

  implicit def commonGenShape[U, T]: FAtomicGenShape[AbstractFAtomicGen[U, T], U, T] = new FAtomicGenShape[AbstractFAtomicGen[U, T], U, T] {
    override def unwrap(input: AbstractFAtomicGen[U, T]): AbstractFAtomicGen[U, T] = input
  }

  implicit def hlistShape[U, Sub, Tail <: HList, SubRe, TailRe <: HList](
    implicit
    sub: FAtomicGenShape[Sub, U, SubRe],
    tail: FAtomicGenShape[Tail, U, TailRe]
  ): FAtomicGenShape[Sub :: Tail, U, SubRe :: TailRe] = new FAtomicGenShape[Sub :: Tail, U, SubRe :: TailRe] {
    override def unwrap(input: Sub :: Tail): AbstractFAtomicGen[U, SubRe :: TailRe] = new AbstractFAtomicGen[U, SubRe :: TailRe] {
      val subInput :: tailInput = input
      override def getBy(atomics: List[FAtomic[U]]): Either[FAtomicException, SubRe :: TailRe] = {
        val subGen = sub.unwrap(subInput)
        val tailGen = tail.unwrap(tailInput)
        (subGen.getBy(atomics): Either[FAtomicException, SubRe]) -> (tailGen.getBy(atomics): Either[FAtomicException, TailRe]) match {
          case (Left(s), Left(t)) =>
            Left(FAtomicException(s.typeTags ::: t.typeTags))
          case (Left(s), Right(_)) =>
            Left(FAtomicException(s.typeTags))
          case (Right(_), Left(s)) =>
            Left(FAtomicException(s.typeTags))
          case (Right(s), Right(t)) =>
            Right(s :: t)
        }
      }
    }
  }

}*/
trait FAtomicGenHelper {

  def needAtomic[U, T[_]](implicit parGen: FAtomicPartialFunctionGen[T], typeTag: WeakTypeTag[T[_]]): FAtomicGen[U, T] = new FAtomicGen[U, T] {
    override def getBy(atomics: List[FAtomic[U]]): Either[FAtomicException, T[U]] = {
      atomics.find(parGen.par[U].isDefinedAt) match {
        case Some(s) =>
          Right(parGen.par[U](s))
        case _ =>
          Left(FAtomicException(List(typeTag)))
      }
    }
  }

  def needAtomicOpt[U, T[_]](implicit parGen: FAtomicPartialFunctionGen[T]): FAtomicGenOpt[U, T] = new FAtomicGenOpt[U, T] {
    override def getBy(atomics: List[FAtomic[U]]): Either[FAtomicException, Option[T[U]]] = {
      Right(atomics.find(parGen.par[U].isDefinedAt).map(parGen.par[U].apply))
    }
  }

  def needAtomicList[U, T[_]](implicit parGen: FAtomicPartialFunctionGen[T]): FAtomicGenList[U, T] = new FAtomicGenList[U, T] {
    override def getBy(atomics: List[FAtomic[U]]): Either[FAtomicException, List[T[U]]] = {
      Right(atomics.filter(parGen.par[U].isDefinedAt).map(parGen.par[U].apply))
    }
  }

}

object FAtomicGenHelper extends FAtomicGenHelper

trait FAtomicPartialFunctionGen[T[_]] {
  def par[U]: PartialFunction[FAtomic[U], T[U]]
}

object FAtomicPartialFunctionGen {

  import scala.language.experimental.macros
  implicit def fCompModel2CParamFormater[T[_]]: FAtomicPartialFunctionGen[T] = macro FAtomicPartialFunctionGenImpl.apply[T]

}

class FAtomicPartialFunctionGenImpl(val c: scala.reflect.macros.blackbox.Context) {
  import c.universe._
  def apply[T[_]](implicit tTag: c.WeakTypeTag[T[_]]): c.Expr[FAtomicPartialFunctionGen[T]] = {
    val entity = c.weakTypeOf[T[_]]
    val typeSymbol = entity.typeSymbol
    val expr = c.Expr[FAtomicPartialFunctionGen[T]](
      q"""
        new _root_.net.scalax.fsn.core.FAtomicPartialFunctionGen[$typeSymbol] {
          override def par[U]: PartialFunction[_root_.net.scalax.fsn.core.FAtomic[U], $typeSymbol[U]] = {
           case (s: $typeSymbol[U @_root_.scala.unchecked]) => s
          }
        }
     """
    )
    //println(expr.toString())
    expr
  }
}