package net.scalax.fsn.core

import scala.language.higherKinds

trait AtomicWrap {
  type DataType
  type CollType[_]
  val atomics: CollType[Atomic[DataType]]
}