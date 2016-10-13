package net.scalax.fsn.json.operation

import io.circe.Json
import io.circe.syntax._
import net.scalax.fsn.common.atomic.DefaultValue
import net.scalax.fsn.core.{FColumn, FsnColumn}
import net.scalax.fsn.json.atomic.{JsonReader, JsonWriter}

object CommonOperation {

  def tryToFillIfEmpty(column: FColumn): FColumn = {
    if (column.data.isDefined) {
      column
    } else {
      val defaultValue = FColumn.findOpt(column) { case s: DefaultValue[column.DataType] => s }
      defaultValue match {
        case Some(s) => FsnColumn(column.cols, Option(s.value))
        case _ => column
      }
    }
  }

  def tryToFillAllIfEmpty(columns: List[FColumn]): List[FColumn] = {
    columns.map(tryToFillIfEmpty)
  }

  def genDataFromFColumn(column: FColumn): Option[column.DataType] = {
    column.data match {
      case dd@Some(s) =>
        dd
      case _ =>
        FColumn.findOpt(column) { case s: DefaultValue[column.DataType] => s }.map(_.value)
    }
  }

}