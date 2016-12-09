package net.scalax.fsn.core

import scala.reflect.runtime.universe._
import scala.language.higherKinds
import scala.language.implicitConversions

case class FAtomicException(typeTags: List[WeakTypeTag[_]]) extends Exception(
  s"""
     |以下类型对应的 FAtomic 没能找到:
     |${typeTags.map(_.tpe.toString).mkString("\n")}
     |
   """.stripMargin) {
  //def append(typeTag: WeakTypeTag[_]): FAtomicException = FAtomicException(typeTag :: typeTags)
  //def appendAll(appendTypeTags: List[WeakTypeTag[_]]): FAtomicException = FAtomicException(appendTypeTags ::: typeTags)
}