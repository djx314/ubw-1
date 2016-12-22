package net.scalax.fsn.core

trait FAtomic[D]

object FAtomicList {

  def empty[D]: List[FAtomic[D]] = Nil

}