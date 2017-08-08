package net.scalax.fsn.core

trait Atomic[D]

object AtomicList {

  def empty[D]: List[Atomic[D]] = Nil

}