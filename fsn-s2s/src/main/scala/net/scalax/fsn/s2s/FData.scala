package net.scalax.fsn.s2s

import slick.lifted._
import scala.language.higherKinds

trait SlickWriter extends FWriter {

  type SourceColumn
  type TargetColumn
  val sourceColumn: SourceColumn

  override type Writer[C] = Shape[_ <: FlatShapeLevel, SourceColumn, C, TargetColumn]

}

trait SlickReader extends FReader {

  type SourceColumn
  type TargetColumn
  val sourceColumn: SourceColumn

  override type Reader[C] = Shape[_ <: FlatShapeLevel, SourceColumn, C, TargetColumn]

}

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