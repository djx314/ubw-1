package net.scalax.fsn.slick.model

case class SlickRange(drop: Int, take: Option[Int])
case class SlickPage(pageIndex: Int, pageSize: Int)
case class ColumnOrder(columnName: String, isDesc: Boolean)

case class SlickParam(orders: List[ColumnOrder] = Nil, range: Option[SlickRange] = None, page: Option[SlickPage] = None)