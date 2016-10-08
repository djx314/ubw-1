package net.scalax.fsn.parameter.helper

import net.scalax.fsn.parameter.atomic.DataMapGen

class ParamGenImpl(val c: scala.reflect.macros.blackbox.Context) {
  import c.universe._
  def apply[Entity: c.WeakTypeTag]: c.Expr[DataMapGen[Entity]] = {
    val entity = c.weakTypeOf[Entity]
    val expr = c.Expr[DataMapGen[Entity]](
      q"""
       new _root_.net.scalax.fsn.parameter.atomic.DataMapGen[$entity] {
         override def apply(s: $entity) = {
           val labelGen = _root_.shapeless.LabelledGeneric[$entity]
           val hlistData = labelGen.to(s)
           import _root_.shapeless.record._
           _root_.shapeless.ops.record.Keys[labelGen.Repr].apply.toList.map(_.name).zip(hlistData.values.toList.map(_.paramComp)).map {
             case (key, value) => value.toContent(key)
           }
         }
       }
     """
    )
    println(expr.toString())
    expr
  }
}

trait ParamGenImplicit {
  import scala.language.experimental.macros
  implicit def fCompModel2CParamFormater[T]: DataMapGen[T] = macro ParamGenImpl.apply[T]
}