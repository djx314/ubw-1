package net.scalax.fsn.mix.slickbase

import net.scalax.fsn.mix.helpers.SlickCRUDImplicits
import net.scalax.fsn.slick.helpers.FilterRepImplicitHelper
import slick.lifted._
import slick.relational.RelationalProfile

trait SqlRepImplicits extends FilterRepImplicitHelper with SlickCRUDImplicits {

  implicit class queryToUQueryExtendsionMethodGen[E, U](query: Query[E, U, Seq]) {

    def out = new ListQueryExtensionMethods[E, U](query)

  }

  implicit class queryToCrudQueryExtendsionMethodGen[E <: RelationalProfile#Table[_], U](query: Query[E, U, Seq]) {

    def in = new CrudQueryExtensionMethods[E, U](query)

  }

}