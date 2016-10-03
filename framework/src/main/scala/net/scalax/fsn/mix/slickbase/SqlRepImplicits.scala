package net.scalax.fsn.mix.slickbase

import net.scalax.fsn.core.FColumnImplicits
import net.scalax.fsn.slick.helpers.FilterRepImplicitHelper
import slick.lifted._
import slick.relational.RelationalProfile

import scala.language.implicitConversions

trait SqlRepImplicits extends FilterRepImplicitHelper with FColumnImplicits {

  implicit class queryToUQueryExtendsionMethodGen[E, U](query: Query[E, U, Seq]) {

    def out = new ListQueryExtensionMethods[E, U](query)

  }

  implicit class queryToCrudQueryExtendsionMethodGen[E <: RelationalProfile#Table[_], U](query: Query[E, U, Seq]) {

    def in = new CrudQueryExtensionMethods[E, U](query)

  }

}