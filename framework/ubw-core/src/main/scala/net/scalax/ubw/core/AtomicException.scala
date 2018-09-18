package net.scalax.ubw.core

import scala.reflect.runtime.universe._

case class AtomicException(typeTags: List[WeakTypeTag[_]]) extends Exception(
  s"""
     |以下类型对应的 Atomic 没能找到:
     |${typeTags.map(_.tpe.toString).mkString("\n")}
     |
   """.stripMargin
) {

  def :::(exception: AtomicException): AtomicException = AtomicException(exception.typeTags ::: typeTags)

}