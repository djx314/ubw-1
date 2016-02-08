package net.scalax.fsn.s2s

import slick.lifted._
import scala.language.implicitConversions

class ColumnExtensionMethods[T, S, R](repLike: T)(implicit sShape: Shape[_ <: FlatShapeLevel, T, S, R]) {

  def setTo[H, B, U](targetRepLike: H)(implicit tShape: Shape[_ <: FlatShapeLevel, H, B, U]): (S => B) => SlickWrapper = {
    (convert1) => {
      val reader1 = new SlickReader {
        override type SourceColumn = T
        override type TargetColumn = R
        override val sourceColumn = repLike
        override type DataType = S
        override val reader = sShape
      }
      val writer1 = new SlickWriter {
        override type SourceColumn = H
        override type TargetColumn = U
        override val sourceColumn = targetRepLike
        override type DataType = B
        override val writer = tShape
      }
      new SlickWrapper {
        override val writer = writer1
        override val reader = reader1
        override val convert = convert1
      }
    }
  }

  def setToSame[H, U](targetRepLike: H)(implicit tShape: Shape[_ <: FlatShapeLevel, H, S, U]): SlickWrapper = {
    setTo(targetRepLike)(tShape)((s: S) => s)
  }

}

trait SlickConvert {

  implicit def columnExtensionMethodImplicit[T, S, R](repLike: T)(implicit sShape: Shape[_ <: FlatShapeLevel, T, S, R]): ColumnExtensionMethods[T, S, R] = {
    new ColumnExtensionMethods(repLike)(sShape)
  }

  implicit class SlickQueryExtensionMethods[E, U](val query: Query[E, U, Seq]) {

    def in: SourceQueryExtensionMethods[E, U] = {
      new SourceQueryExtensionMethods(query)
    }

    def out: TargetQueryExtensionMethods[E, U] = {
      new TargetQueryExtensionMethods(query)
    }

  }

}