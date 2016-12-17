package net.scalax.fsn.core

import scala.reflect.runtime.universe._

case class FAtomicException(typeTags: List[WeakTypeTag[_]]) extends Exception(
  s"""
     |以下类型对应的 FAtomic 没能找到:
     |${typeTags.map(_.tpe.toString).mkString("\n")}
     |
   """.stripMargin) {

  def :::(exception: FAtomicException): FAtomicException = FAtomicException(exception.typeTags ::: typeTags)

}