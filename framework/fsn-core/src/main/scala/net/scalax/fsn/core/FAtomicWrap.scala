package net.scalax.fsn.core

import scala.language.higherKinds

trait FAtomicWrap {
  type DataType
  type CollType[_]
  val atomics: CollType[FAtomic[DataType]]
}