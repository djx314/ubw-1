package net.scalax.fsn.s2s

import net.scalax.fsn.core._

import slick.lifted._
import scala.language.higherKinds

trait SlickWriter extends FWriter {

  type SourceColumn
  type TargetColumn
  val sourceColumn: SourceColumn

  override type Writer[C] = Shape[_ <: FlatShapeLevel, SourceColumn, C, TargetColumn]

}

object SlickWriter {

  def apply[A, B, C <: FlatShapeLevel, D](repLike: A)(shape: Shape[C, A, B, D]) = {
    new SlickWriter {
      override type SourceColumn = A
      override type TargetColumn = D
      override type DataType = B
      override val sourceColumn = repLike
      override val writer = shape
    }
  }

}

trait SlickReader extends FReader {

  type SourceColumn
  type TargetColumn
  val sourceColumn: SourceColumn

  override type Reader[C] = Shape[_ <: FlatShapeLevel, SourceColumn, C, TargetColumn]

}

object SlickReader {

  def apply[A, B, C <: FlatShapeLevel, D](repLike: A)(shape: Shape[C, A, B, D]) = {
    new SlickReader {
      override type SourceColumn = A
      override type TargetColumn = D
      override type DataType = B
      override val sourceColumn = repLike
      override val reader = shape
    }
  }

}