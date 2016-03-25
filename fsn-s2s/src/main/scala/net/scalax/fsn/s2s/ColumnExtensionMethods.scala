package net.scalax.fsn.s2s

import scala.language.implicitConversions
import shapeless._
import slick.lifted._

trait SlickConvert {

  protected class ColumnExtensionMethods[T, S, R](repLike: T)(implicit sShape: Shape[_ <: FlatShapeLevel, T, S, R]) {

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


  trait HListReader[T, H] {

    type SourceColumn
    type DataType
    type TargetColumn
    val shape: Shape[_ <: FlatShapeLevel, SourceColumn, DataType, TargetColumn]
    val colConvert: T => SourceColumn
    val convert: DataType => H

  }

  implicit def aaaaaaaabbimplicit[U, V, W <: HList, X, Y, Z <: HList](implicit bconvert: U <:< (V :: W), bShape: Shape[_ <: FlatShapeLevel, V, X, Y], hListReader: HListReader[W, Z]): HListReader[U, X :: Z] = {
    new HListReader[U, X :: Z] {
      override type SourceColumn = (V, hListReader.SourceColumn)
      override type DataType = (X, hListReader.DataType)
      override type TargetColumn = (Y, hListReader.TargetColumn)
      override val shape = new TupleShape[FlatShapeLevel, SourceColumn, DataType, TargetColumn](bShape, hListReader.shape)
      override val colConvert = (s: U) => {
        val vCol :: wCol = bconvert(s)
        vCol -> hListReader.colConvert(wCol)
      }
      override val convert = (s: DataType) => s._1 :: hListReader.convert(s._2)
    }
  }

  implicit def kkk[X, Y, Z, W](implicit bconvert: X <:< (Y :: HNil), bShape: Shape[_ <: FlatShapeLevel, Y, Z, W]): HListReader[X, Z :: HNil] = {
    new HListReader[X, Z :: HNil] {
      override type SourceColumn = Y
      override type DataType = Z
      override type TargetColumn = W
      override val shape = bShape
      override val colConvert = (s: X) => bconvert(s)(0)
      override val convert = (s: Z) => s :: HNil
    }
  }

  implicit class ColumnExtensionMethods1111[T, H](repLike: T)(implicit hlistReader: HListReader[T, H]) {

    def setTo[K, B, U](targetRepLike: K)(implicit tShape: Shape[_ <: FlatShapeLevel, K, B, U]): (H => B) => SlickWrapper = {
      (convert1) => {
        val reader1 = new SlickReader {
          override type SourceColumn = hlistReader.SourceColumn
          override type TargetColumn = hlistReader.TargetColumn
          override val sourceColumn = hlistReader.colConvert(repLike)
          override type DataType = hlistReader.DataType
          override val reader = hlistReader.shape
        }
        val writer1 = new SlickWriter {
          override type SourceColumn = K
          override type TargetColumn = U
          override val sourceColumn = targetRepLike
          override type DataType = B
          override val writer = tShape
        }
        new SlickWrapper {
          override val writer = writer1
          override val reader = reader1
          override val convert = (sourceData: hlistReader.DataType) => {
            convert1(hlistReader.convert(sourceData))
          }
        }
      }
    }

  }

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