package net.scalax.fsn.core

import scala.reflect.runtime.universe._
import scala.language.higherKinds

trait AbstractFAtomicGen[U, T] {
  def getBy(atomics: List[FAtomic[U]]): Either[FAtomicException, T]
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

/*trait FAtomicPartialFunctionGenBase[T[_]] {
  def par[U]: PartialFunction[FAtomic[U], T[U]]
}*/

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
           case s: $typeSymbol[U] => s
          }
        }
     """
    )
    //println(expr.toString())
    expr
  }
}