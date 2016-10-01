package net.scalax.fsn.slick.helpers

import slick.ast.{ColumnOption, TypedType}

import scala.language.existentials
import scala.language.implicitConversions
import slick.lifted._
import slick.relational.RelationalProfile

trait FsnTable {
  self: RelationalProfile#Table[_] =>

  def fsnColumn[C](n: String, options: ColumnOption[C]*)(implicit tt: TypedType[C]): FRep[Rep[C]] = {
    new FRep[Rep[C]] {
      override val owner = self
      override val rep = self.column[C](n, options: _*)(tt)
    }
  }

}

trait FRep[C] {

  val owner: RelationalProfile#Table[_]
  val rep: C

}

object FRep {

  implicit def fRep2CommonRep[C](fRep: FRep[C]): C = fRep.rep

  def apply[C](repLike: C, owner1: RelationalProfile#Table[_]): FRep[C] = {
    new FRep[C] {
      override val owner = owner1
      override val rep = repLike
    }
  }

}