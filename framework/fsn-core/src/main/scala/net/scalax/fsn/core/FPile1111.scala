package net.scalax.fsn.core

import net.scalax.fsn.core.ListUtils.WeightData

sealed abstract trait FPileAbs1111 {
  self =>
  type DataType

  def dataLengthSum: Int = {
    self match {
      case pList: FPileList =>
        pList.encodePiles(pList.pileEntity).map(_.dataLengthSum).sum
      case pile: FPile1111 =>
        if (pile.subs.isEmpty)
          pile.fShape.dataLength
        else
          pile.subs.map(_.dataLengthSum).sum
    }
  }

  def dataListFromSubList(atomicDatas: List[WeightData[FAtomicValue]]): List[WeightData[FAtomicValue]] = {
    ???
  }
}

trait FPileList extends FPileAbs1111 {
  type PileType
  override type DataType

  val pileEntity: PileType

  def encodePiles(piles: PileType): List[FPile]
  def decodePileData(datas: List[Any]): DataType
  def encodePileData(data: DataType): List[Any]

}

case class FPileListImpl[PT, DT](
    override val pileEntity: PT,
    encoder: PT => List[FPile],
    dataDecoder: List[Any] => DT,
    dataEncoder: DT => List[Any]
) extends FPileList {
  override type PileType = PT
  override type DataType = DT

  override def encodePiles(piles: PT): List[FPile] = encoder(piles)
  override def decodePileData(datas: List[Any]): DT = dataDecoder(datas)
  override def encodePileData(data: DataType): List[Any] = dataEncoder(data)
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