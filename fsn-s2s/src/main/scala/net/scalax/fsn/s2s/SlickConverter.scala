package net.scalax.fsn.s2s

import net.scalax.fsn.core._

import slick.lifted._

trait SlickConverter extends FConverter {

  override type Writer = SlickWriter

  override type Reader = SlickReader

  override val writer: SlickWriter

  override val reader: SlickReader

  val convert: reader.DataType => writer.DataType

  def append(slickConverter: SlickConverter): SlickConverter = {

    type RSourceColumn = (this.reader.SourceColumn, slickConverter.reader.SourceColumn)
    type RTargetColumn = (this.reader.TargetColumn, slickConverter.reader.TargetColumn)
    val rSourceColumn = this.reader.sourceColumn -> slickConverter.reader.sourceColumn
    type RDataType = (this.reader.DataType, slickConverter.reader.DataType)
    val rShape = new TupleShape[FlatShapeLevel, RSourceColumn, RDataType, RTargetColumn](this.reader.reader, slickConverter.reader.reader)
    val newReader = SlickReader(rSourceColumn)(rShape)

    type WSourceColumn = (this.writer.SourceColumn, slickConverter.writer.SourceColumn)
    type WTargetColumn = (this.writer.TargetColumn, slickConverter.writer.TargetColumn)
    val wSourceColumn = this.writer.sourceColumn -> slickConverter.writer.sourceColumn
    type WDataType = (this.writer.DataType, slickConverter.writer.DataType)
    val wShape = new TupleShape[FlatShapeLevel, WSourceColumn, WDataType, WTargetColumn](this.writer.writer, slickConverter.writer.writer)
    val newWriter = SlickWriter(wSourceColumn)(wShape)

    val newConvert: RDataType => WDataType = (rData) => {
      val f1 = this.convert(rData._1)
      val f2 = slickConverter.convert(rData._2)
      f1 -> f2
    }

    new SlickConverter {
      override val reader = newReader
      override val writer = newWriter
      override val convert = newConvert
    }
  }

}