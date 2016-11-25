package net.scalax.fsn.core

import scala.reflect.runtime.universe._
import scala.language.higherKinds
import scala.language.implicitConversions

trait AbstractFAtomicGen {
  type T[_]
  def gen[U](atomics: List[FAtomic[U]]): T[U]
}

trait FAtomicGen[S[_]] extends AbstractFAtomicGen {
  override type T[K] = S[K]
}

trait FAtomicGenOpt[S[_]] extends AbstractFAtomicGen {
  override type T[K] = Option[S[K]]
}

trait FAtomicGenImpl {

  def needAtomic[T[_]](implicit parGen: FAtomicPartialFunctionGen[T], typeTag: WeakTypeTag[T[_]]): FAtomicGen[T] = new FAtomicGen[T] {
    //override val weakTypeTag = typeTag
    override def gen[U](atomics: List[FAtomic[U]]): T[U] = {
      atomics.find(parGen.par[U].isDefinedAt).map(parGen.par[U].apply).getOrElse(throw new Exception(s"找不到匹配类型 ${typeTag.tpe} 的转换器"))
    }
  }

  def needAtomicOpt[T[_]](implicit parGen: FAtomicPartialFunctionGen[T]): FAtomicGenOpt[T] = new FAtomicGenOpt[T] {
    //override val weakTypeTag = typeTag
    override def gen[U](atomics: List[FAtomic[U]]): T[U] = {
      atomics.find(parGen.par[U].isDefinedAt).map(parGen.par[U].apply)
    }
  }

}

//目前的思路
trait FAtomicPartialFunctionGen[T[_]] {
  def par[U]: PartialFunction[FAtomic[U], T[U]]
}
//错误的思路
/*trait FAtomicPartialFunctionGen[T] {
  def par(path: FPath): PartialFunction[FAtomic[path.DataType], T]
}*/

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