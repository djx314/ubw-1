package net.scalax.fsn.core

import shapeless.{ ::, HList, HNil }

import scala.language.higherKinds
import scala.language.implicitConversions
import scala.reflect.runtime.universe._

trait AbstractAtomicGen[U, S] {
  def getBy(atomics: List[Atomic[U]]): Either[AtomicException, S]
}

object AbstractAtomicGen {
  def empty[U]: AbstractAtomicGen[U, HNil] = new AbstractAtomicGen[U, HNil] {
    def getBy(atomics: List[Atomic[U]]): Either[AtomicException, HNil] = Right(HNil)
  }

  class bbbb[T, F <: HList](bb: AbstractAtomicGen[T, F]) {
    def ::[G](cc: AbstractAtomicGen[T, G]): AbstractAtomicGen[T, G :: F] = {
      new AbstractAtomicGen[T, G :: F] {
        override def getBy(atomics: List[Atomic[T]]): Either[AtomicException, G :: F] = {
          (cc.getBy(atomics), bb.getBy(atomics)) match {
            case (Left(e1), Left(e2)) =>
              Left(AtomicException(e1.typeTags ::: e2.typeTags))
            case (Left(e1), Right(_)) =>
              Left(AtomicException(e1.typeTags))
            case (Right(_), Left(e2)) =>
              Left(AtomicException(e2.typeTags))
            case (Right(out1), Right(out2)) =>
              Right(out1 :: out2)
          }
        }
      }
    }
  }

  implicit def convert[T, F <: HList](bb: AbstractAtomicGen[T, F]): bbbb[T, F] = new bbbb(bb)
}

trait AtomicGen[U, S[_]] extends AbstractAtomicGen[U, S[U]] {
  override def getBy(atomics: List[Atomic[U]]): Either[AtomicException, S[U]]
}

trait AtomicGenOpt[U, S[_]] extends AbstractAtomicGen[U, Option[S[U]]] {
  override def getBy(atomics: List[Atomic[U]]): Either[AtomicException, Option[S[U]]]
}

trait AtomicGenList[U, S[_]] extends AbstractAtomicGen[U, List[S[U]]] {
  override def getBy(atomics: List[Atomic[U]]): Either[AtomicException, List[S[U]]]
}
/*trait AtomicGenShape[-Input, U, S] {
  def unwrap(input: Input): AbstractAtomicGen[U, S]
}

object AtomicGenShape extends AtomicGenShapeImpl {

}

trait AtomicGenShapeImpl {
  implicit def hnilShape[U]: AtomicGenShape[HNil, U, HNil] = new AtomicGenShape[HNil, U, HNil] {
    override def unwrap(input: HNil): AbstractAtomicGen[U, HNil] = new AbstractAtomicGen[U, HNil] {
      override def getBy(atomics: List[Atomic[U]]): Either[AtomicException, HNil] = {
        Right(HNil)
      }
    }
  }

  implicit def commonGenShape[U, T]: AtomicGenShape[AbstractAtomicGen[U, T], U, T] = new AtomicGenShape[AbstractAtomicGen[U, T], U, T] {
    override def unwrap(input: AbstractAtomicGen[U, T]): AbstractAtomicGen[U, T] = input
  }

  implicit def hlistShape[U, Sub, Tail <: HList, SubRe, TailRe <: HList](
    implicit
    sub: AtomicGenShape[Sub, U, SubRe],
    tail: AtomicGenShape[Tail, U, TailRe]
  ): AtomicGenShape[Sub :: Tail, U, SubRe :: TailRe] = new AtomicGenShape[Sub :: Tail, U, SubRe :: TailRe] {
    override def unwrap(input: Sub :: Tail): AbstractAtomicGen[U, SubRe :: TailRe] = new AbstractAtomicGen[U, SubRe :: TailRe] {
      val subInput :: tailInput = input
      override def getBy(atomics: List[Atomic[U]]): Either[AtomicException, SubRe :: TailRe] = {
        val subGen = sub.unwrap(subInput)
        val tailGen = tail.unwrap(tailInput)
        (subGen.getBy(atomics): Either[AtomicException, SubRe]) -> (tailGen.getBy(atomics): Either[AtomicException, TailRe]) match {
          case (Left(s), Left(t)) =>
            Left(AtomicException(s.typeTags ::: t.typeTags))
          case (Left(s), Right(_)) =>
            Left(AtomicException(s.typeTags))
          case (Right(_), Left(s)) =>
            Left(AtomicException(s.typeTags))
          case (Right(s), Right(t)) =>
            Right(s :: t)
        }
      }
    }
  }

}*/
trait AtomicGenHelper {

  def needAtomic[U, T[_]](implicit parGen: AtomicPartialFunctionGen[T], typeTag: WeakTypeTag[T[_]]): AtomicGen[U, T] = new AtomicGen[U, T] {
    override def getBy(atomics: List[Atomic[U]]): Either[AtomicException, T[U]] = {
      atomics.find(parGen.par[U].isDefinedAt) match {
        case Some(s) =>
          Right(parGen.par[U](s))
        case _ =>
          Left(AtomicException(List(typeTag)))
      }
    }
  }

  def needAtomicOpt[U, T[_]](implicit parGen: AtomicPartialFunctionGen[T]): AtomicGenOpt[U, T] = new AtomicGenOpt[U, T] {
    override def getBy(atomics: List[Atomic[U]]): Either[AtomicException, Option[T[U]]] = {
      Right(atomics.find(parGen.par[U].isDefinedAt).map(parGen.par[U].apply))
    }
  }

  def needAtomicList[U, T[_]](implicit parGen: AtomicPartialFunctionGen[T]): AtomicGenList[U, T] = new AtomicGenList[U, T] {
    override def getBy(atomics: List[Atomic[U]]): Either[AtomicException, List[T[U]]] = {
      Right(atomics.filter(parGen.par[U].isDefinedAt).map(parGen.par[U].apply))
    }
  }

}

object AtomicGenHelper extends AtomicGenHelper

trait AtomicPartialFunctionGen[T[_]] {
  def par[U]: PartialFunction[Atomic[U], T[U]]
}

object AtomicPartialFunctionGen {

  import scala.language.experimental.macros
  implicit def fCompModel2CParamFormater[T[_]]: AtomicPartialFunctionGen[T] = macro AtomicPartialFunctionGenImpl.apply[T]

}

class AtomicPartialFunctionGenImpl(val c: scala.reflect.macros.blackbox.Context) {
  import c.universe._
  def apply[T[_]](implicit tTag: c.WeakTypeTag[T[_]]): c.Expr[AtomicPartialFunctionGen[T]] = {
    val entity = c.weakTypeOf[T[_]]
    val typeSymbol = entity.typeSymbol
    val expr = c.Expr[AtomicPartialFunctionGen[T]](
      q"""
        new _root_.net.scalax.fsn.core.AtomicPartialFunctionGen[$typeSymbol] {
          override def par[U]: PartialFunction[_root_.net.scalax.fsn.core.Atomic[U], $typeSymbol[U]] = {
           case (s: $typeSymbol[U @_root_.scala.unchecked]) => s
          }
        }
     """
    )
    //println(expr.toString())
    expr
  }
}