package net.scalax.fsn.slick.helpers

import slick.relational.RelationalProfile

trait FRep[C] {

  val owner: RelationalProfile#Table[_]
  val rep: C

}

object FRep {

  //implicit def fRep2CommonRep[C](fRep: FRep[C]): C = fRep.rep

  def apply[C](repLike: C, owner1: RelationalProfile#Table[_]): FRep[C] = {
    new FRep[C] {
      override val owner = owner1
      override val rep = repLike
    }
  }

}