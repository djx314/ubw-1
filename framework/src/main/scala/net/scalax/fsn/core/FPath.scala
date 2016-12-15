package net.scalax.fsn.core

import scala.reflect.runtime.universe._
import scala.language.higherKinds
import scala.language.implicitConversions

trait FPath {
  type DataType
  val atomics: List[FAtomic[DataType]]

  override def toString: String = {
    s"""|paths:
       |${ atomics.map(s => "  " + s.toString).mkString("\n") }""".stripMargin
  }
}

case class FPathImpl[D](override val atomics: List[FAtomic[D]]) extends FPath {
  override type DataType = D
}

object FPath {
  def findOpt[T](path: FPath)(par: PartialFunction[FAtomic[path.DataType], T]): Option[T] = {
    path.atomics.find(par.isDefinedAt).map(par.apply)
  }

  def find[T](path: FPath)(par: PartialFunction[FAtomic[path.DataType], T])(typeTag: WeakTypeTag[T]): T = {
    findOpt(path)(par).getOrElse(throw new Exception(s"找不到匹配类型 ${typeTag.tpe} 的转换器"))
  }

  def filter[T](path: FPath)(par: PartialFunction[FAtomic[path.DataType], T]): List[T] = {
    path.atomics.filter(par.isDefinedAt).map(par.apply)
  }

  def translate[T](pathTran: FPath => T)(implicit aa: String) = {

  }
}
/*trait FPathShape[Source, Target] {

  def apply(path: FPath, source: Source): Target

}*/