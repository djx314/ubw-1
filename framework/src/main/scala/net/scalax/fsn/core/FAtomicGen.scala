package net.scalax.fsn.core

import scala.reflect.runtime.universe._
import scala.language.higherKinds
import scala.language.implicitConversions

trait AbstractFAtomicGen {
  type T
  //val weakTypeTag: WeakTypeTag[T[_]]
  def gen(path: FPath): T
}

trait FAtomicGen[S] extends AbstractFAtomicGen {
  override type T = S
}

trait FAtomicGenImpl {

  def needAtomic[T](implicit parGen: FAtomicPartialFunctionGen[T], typeTag: WeakTypeTag[T]): FAtomicGen[T] = new FAtomicGen[T] {
    //override val weakTypeTag = typeTag
    override def gen(path: FPath): T = {
      FPath.find(path)(parGen.par(path))(typeTag)
    }
  }

}

trait FAtomicPartialFunctionGen[T] {
  def par(path: FPath): PartialFunction[FAtomic[path.DataType], T]
}

object FAtomicPartialFunctionGen {

  import scala.language.experimental.macros
  implicit def fCompModel2CParamFormater[T]: FAtomicPartialFunctionGen[T] = macro FAtomicPartialFunctionGenImpl.apply[T]

}

class FAtomicPartialFunctionGenImpl(val c: scala.reflect.macros.blackbox.Context) {
  import c.universe._
  def apply[T: c.WeakTypeTag]: c.Expr[FAtomicPartialFunctionGen[T]] = {
    val entity = c.weakTypeOf[T]
    val typeSymbol = entity.typeSymbol
    val expr = c.Expr[FAtomicPartialFunctionGen[T]](
      q"""
        new _root_.net.scalax.fsn.core.FAtomicPartialFunctionGen[$entity] {
          override def par(path: _root_.net.scalax.fsn.core.FPath): PartialFunction[_root_.net.scalax.fsn.core.FAtomic[path.DataType], $entity] = {
           case s: $entity => s
          }
        }
     """
    )
    println(expr.toString())
    expr
  }
}