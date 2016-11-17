package net.scalax.fsn.core

trait AbstractFPile {

  val columns: List[FColumn]

}

trait FPile[E, U] extends AbstractFPile {

  val shape: FsnShape[E, U]

}

trait FsnShape[Packed_, UnPacked_] {

  type Packed = Packed_
  type UnPacked = UnPacked_

  def encodeColumn(pile: Packed_): List[FColumn]
  def decodeColumn(columns: List[FColumn]): Packed_

  def encodeData(pileData: UnPacked_): List[Any]
  def decodeData(data: List[Any]): UnPacked_

}