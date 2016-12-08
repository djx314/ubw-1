package net.scalax.fsn.core

import scala.reflect.runtime.universe._
import scala.language.higherKinds
import scala.language.implicitConversions

case class FAtomicException(typeTags: List[WeakTypeTag[_]]) extends Exception {

  def append(typeTag: WeakTypeTag[_]): FAtomicException = FAtomicException(typeTag :: typeTags)

  def appendAll(appendTypeTags: List[WeakTypeTag[_]]): FAtomicException = FAtomicException(appendTypeTags ::: typeTags)

}