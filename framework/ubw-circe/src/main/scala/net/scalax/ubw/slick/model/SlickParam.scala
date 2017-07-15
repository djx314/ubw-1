package net.scalax.fsn.slick.model

import io.circe.Json

case class SlickRange(drop: Int, take: Option[Int])
case class SlickPage(pageIndex: Int, pageSize: Int)
case class ColumnOrder(columnName: String, isDesc: Boolean)

case class SlickParam(orders: List[ColumnOrder] = Nil, range: Option[SlickRange] = None, page: Option[SlickPage] = None, filter: Map[String, Json] = Map.empty)

case class GroupColumn(property: String, method: String)
case class GroupParam(keys: List[String] = Nil, aggregates: List[GroupColumn] = Nil, orders: List[ColumnOrder] = Nil)