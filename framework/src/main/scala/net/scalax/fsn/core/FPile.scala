package net.scalax.fsn.core

trait FPile {

  val columns: List[FColumn]
  val transform: List[FPile] => FPile
  val sourcePiles: List[FPile]

}