package net.scalax.fsn.slick.model

import slick.lifted.{CanBeQueryCondition, Query, Rep}

trait FilterWrapper[E] {
  type Target <: Rep[_]
  val condition: CanBeQueryCondition[Target]
  val convert: E => Target

  def genFilter[U](query: Query[E, U, Seq]): Query[E, U, Seq] = {
    query.filter(data => convert(data))(condition)
  }
}