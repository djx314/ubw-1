package net.scalax.fsn.core

sealed abstract trait FPileAbs1111 {
  type DataType
}

trait FPileList extends FPileAbs1111 {
  type PileType
  override type DataType

  val pileEntity: PileType

  def encodePiles(piles: PileType): List[FPile]
  def decodeData(datas: List[Any]): DataType
}

case class FPileListImpl[PT, DT](
    override val pileEntity: PT,
    encoder: PT => List[FPile],
    decoder: List[Any] => DT
) extends FPileList {
  override type PileType = PT
  override type DataType = DT

  override def encodePiles(piles: PT): List[FPile] = encoder(piles)
  override def decodeData(datas: List[Any]): DT = decoder(datas)
}

trait FPile1111 extends FPileAbs1111 {
  type PathType
  override type DataType

  val pathPile: PathType

  val fShape: FsnShape[PathType, DataType]

  val dataFromSub: List[Any] => DataType
  val subs: List[FPileAbs1111]
}

case class FPile1111Impl[PT, DT](
    override val pathPile: PT,
    override val fShape: FsnShape[PT, DT],
    override val dataFromSub: List[Any] => DT,
    override val subs: List[FPileAbs1111]
) extends FPile1111 {
  override type PathType = PT
  override type DataType = DT
}