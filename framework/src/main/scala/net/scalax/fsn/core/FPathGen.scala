package net.scalax.fsn.core

import scala.reflect.runtime.universe._
import scala.language.higherKinds
import scala.language.implicitConversions

trait AbstractFPathGen {
  type T[_]
  val weakTypeTag: WeakTypeTag[T[_]]
  def gen(path: FPath): T[path.DataType]
}

trait FPathGen[S[_]] extends AbstractFPathGen {
  override type T[A] = S[A]
}

trait FPathGenImpl {

  def needAtomic[S[_]](implicit parGen: FPathPartialFunctionGen[S], typeTag: WeakTypeTag[S[_]]): FPathGen[S] = new FPathGen[S] {
    override val weakTypeTag = typeTag
    override def gen(path: FPath): T[path.DataType] = {
      FPath.find(path)(parGen.par(path))(typeTag)
    }
  }

}

trait FPathPartialFunctionGen[T[_]] {
  def par(path: FPath): PartialFunction[FAtomic[path.DataType], T[path.DataType]]
}

object FPathPartialFunctionGen {

  import scala.language.experimental.macros
  implicit def fCompModel2CParamFormater[T[_]]: FPathPartialFunctionGen[T] = macro FPathPartialFunctionGenImpl.apply[T]

}

class FPathPartialFunctionGenImpl(val c: scala.reflect.macros.blackbox.Context) {
  import c.universe._
  def apply[T[_]](implicit wTypeTag: c.WeakTypeTag[T[_]]): c.Expr[FPathPartialFunctionGen[T]] = {
    val entity = c.weakTypeOf[T[_]]
    val typeSymbol = entity.typeSymbol
    val expr = c.Expr[FPathPartialFunctionGen[T]](
      q"""
        new _root_.net.scalax.fsn.core.FPathPartialFunctionGen[$typeSymbol] {
          override def par(path: _root_.net.scalax.fsn.core.FPath): PartialFunction[_root_.net.scalax.fsn.core.FAtomic[path.DataType], $typeSymbol[path.DataType]] = {
           case s: $typeSymbol[path.DataType] => s
          }
        }
     """
    )
    println(expr.toString())
    expr
  }
}