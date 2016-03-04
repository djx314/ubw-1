package net.scalax.fsn.s2s

import slick.lifted._

trait SlickWrapper {

  val writer: SlickWriter

  val reader: SlickReader

  val convert: reader.DataType => writer.DataType

  def append(slickWrapper: SlickWrapper): SlickWrapper = {

    type RSourceColumn = (this.reader.SourceColumn, slickWrapper.reader.SourceColumn)
    type RTargetColumn = (this.reader.TargetColumn, slickWrapper.reader.TargetColumn)
    val rSourceColumn = this.reader.sourceColumn -> slickWrapper.reader.sourceColumn
    type RDataType = (this.reader.DataType, slickWrapper.reader.DataType)
    val rShape = new TupleShape[FlatShapeLevel, RSourceColumn, RDataType, RTargetColumn](this.reader.reader, slickWrapper.reader.reader)
    val newReader = new SlickReader {
      override type SourceColumn = RSourceColumn
      override type TargetColumn = RTargetColumn
      override type DataType = RDataType
      override val reader = rShape
      override val sourceColumn = rSourceColumn
    }

    type WSourceColumn = (this.writer.SourceColumn, slickWrapper.writer.SourceColumn)
    type WTargetColumn = (this.writer.TargetColumn, slickWrapper.writer.TargetColumn)
    val wSourceColumn = this.writer.sourceColumn -> slickWrapper.writer.sourceColumn
    type WDataType = (this.writer.DataType, slickWrapper.writer.DataType)
    val wShape = new TupleShape[FlatShapeLevel, WSourceColumn, WDataType, WTargetColumn](this.writer.writer, slickWrapper.writer.writer)
    val newWriter = new SlickWriter {
      override type SourceColumn = WSourceColumn
      override type TargetColumn = WTargetColumn
      override type DataType = WDataType
      override val writer = wShape
      override val sourceColumn = wSourceColumn
    }

    val newConvert: RDataType => WDataType = (rData) => {
      val f1 = this.convert(rData._1)
      val f2 = slickWrapper.convert(rData._2)
      f1 -> f2
    }

    new SlickWrapper {
      override val reader = newReader
      override val writer = newWriter
      override val convert = newConvert
    }
  }

}