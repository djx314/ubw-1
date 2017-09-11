package net.scalax.ubw.slick.helpers

import slick.lifted.Query

trait SlickQueryBindImpl {
  def bind[E, U](query: Query[E, U, Seq]): Query[E, U, Seq]
}