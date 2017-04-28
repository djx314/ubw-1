package net.scalax.fsn.database.test

import slick.jdbc.H2Profile.api._

object Sample02 {

def sortByName(query: Query[FriendTable, FriendTable#TableElementType, Seq], colName: String, isDesc: Boolean): Query[FriendTable, FriendTable#TableElementType, Seq] = {
  import slick.lifted.{ Ordered => SlickOrdered }

  val repToOrder = { friend: FriendTable =>
    val order = colName match {
      case "id" => friend.id: SlickOrdered
      case "name" => friend.name: SlickOrdered
      case "nick" => friend.nick: SlickOrdered
      case "age" => friend.age: SlickOrdered
      case "grade" => friend.grade: SlickOrdered
      case _ => throw new IllegalArgumentException("没有匹配的数据库列")
    }
    if (isDesc) {
      new SlickOrdered(order.columns.map(s => s.copy(_2 = s._2.desc)))
    } else {
      new SlickOrdered(order.columns.map(s => s.copy(_2 = s._2.asc)))
    }
  }
  query.sortBy(repToOrder)
}

}