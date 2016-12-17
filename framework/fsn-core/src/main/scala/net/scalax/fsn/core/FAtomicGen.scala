package net.scalax.fsn.core

import scala.reflect.runtime.universe._
import scala.language.higherKinds

trait AbstractFAtomicGen {
  type T[_]
  def getBy[U](atomics: List[FAtomic[U]]): Either[FAtomicException, T[U]]
}

trait FAtomicGen[S[_]] extends AbstractFAtomicGen {
  override type T[K] = S[K]
}

trait FAtomicGenOpt[S[_]] extends AbstractFAtomicGen {
  override type T[K] = Option[S[K]]
}

trait FAtomicGenHelper {

  def needAtomic[T[_]](implicit parGen: FAtomicPartialFunctionGen[T], typeTag: WeakTypeTag[T[_]]): FAtomicGen[T] = new FAtomicGen[T] {
    override def getBy[U](atomics: List[FAtomic[U]]): Either[FAtomicException, T[U]] = {
      atomics.find(parGen.par[U].isDefinedAt) match {
        case Some(s) =>
          Right(parGen.par[U](s))
        case _ =>
          Left(FAtomicException(List(typeTag)))
      }
    }
  }

  def needAtomicOpt[T[_]](implicit parGen: FAtomicPartialFunctionGen[T]): FAtomicGenOpt[T] = new FAtomicGenOpt[T] {
    override def getBy[U](atomics: List[FAtomic[U]]): Either[FAtomicException, T[U]] = {
      Right(atomics.find(parGen.par[U].isDefinedAt).map(parGen.par[U].apply))
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
           case s: $typeSymbol[U] => s
          }
        }
     """
    )
    println(expr.toString())
    expr
  }
}