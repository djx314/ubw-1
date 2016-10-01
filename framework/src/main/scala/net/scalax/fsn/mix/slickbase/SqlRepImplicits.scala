package net.scalax.fsn.mix.slickbase

import aaaa.FilterRepImplicit1111
import net.scalax.fsn.core.FColumnImplicits
import slick.lifted._
import slick.relational.RelationalProfile

import scala.language.implicitConversions

trait SqlRepImplicits extends FilterRepImplicit1111 with FColumnImplicits {

  implicit class queryToUQueryExtendsionMethodGen[E, U](query: Query[E, U, Seq]) {

    def out = new ListQueryExtensionMethods[E, U](query)

  }

  implicit class queryToCrudQueryExtendsionMethodGen[E <: RelationalProfile#Table[_], U](query: Query[E, U, Seq]) {

    def in = new CrudQueryExtensionMethods[E, U](query)

  }

}