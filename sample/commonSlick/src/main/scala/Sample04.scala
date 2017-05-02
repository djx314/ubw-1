package net.scalax.fsn.database.test

import slick.jdbc.H2Profile.api._

object Sample04 {

  def queryFilter(query: Query[FriendTable, FriendTable#TableElementType, Seq], age: Option[Int], grade: Option[Int]): Query[FriendTable, FriendTable#TableElementType, Seq] = {
    val query1 = age match {
      case Some(s) => query.filter(_.age === s)
      case _ => query
    }
    grade match {
      case Some(s) => query1.filter(_.grade === s)
      case _ => query1
    }
  }

}