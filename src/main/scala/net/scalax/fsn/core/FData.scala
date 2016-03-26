package net.scalax.fsn.core

import scala.language.higherKinds

trait FWriter {

  type DataType

  type Writer[_]

  val writer: Writer[DataType]

}

trait FReader {

  type DataType

  type Reader[_]

  val reader: Reader[DataType]

}

trait FConverter {

  type Reader <: FReader
  type Writer <: FWriter

  val reader: Reader

  val writer: Writer

  val convert: reader.DataType => writer.DataType

}