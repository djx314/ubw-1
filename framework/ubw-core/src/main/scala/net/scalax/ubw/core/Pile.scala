package net.scalax.fsn.core

import scala.language.higherKinds

sealed abstract trait Pile {
  self =>
  type DataType

  def leafZero: List[DataPile]

}

trait PileList extends Pile {
  self =>

  type PileType
  override type DataType

  def leafZero: List[DataPile] = {
    encodePiles.map(_.leafZero).flatten
  }

  val pileEntity: PileType

  def encodePiles: List[CommonPile]

  def decodePileData(datas: List[Any]): DataType
  def encodePileData(data: DataType): List[Any]

}

class PileListImpl[PT, DT](
    override val pileEntity: PT,
    encoder: List[CommonPile],
    dataDecoder: List[Any] => DT,
    dataEncoder: DT => List[Any]
) extends PileList {
  override type PileType = PT
  override type DataType = DT

  override def encodePiles: List[CommonPile] = encoder
  override def decodePileData(data: List[Any]): DT = dataDecoder(data)
  override def encodePileData(data: DataType): List[Any] = dataEncoder(data)
}

abstract trait CommonPile extends Pile {
  self =>

  type PathType
  override type DataType

  val pathPile: PathType
  val fShape: PileShape[PathType, DataType]

}

trait BranchPile extends CommonPile {
  self =>

  val subs: Pile
  def dataFromSub(subDatas: Any): DataType

  def leafZero: List[DataPile] = {
    subs.leafZero
  }

}

class BranchPileImpl[PT, DT](
    override val pathPile: PT,
    override val fShape: PileShape[PT, DT],
    override val subs: Pile,
    dataFromSubFunc: Any => DT
) extends BranchPile {
  override type PathType = PT
  override type DataType = DT

  override def dataFromSub(subDatas: Any): DataType = dataFromSubFunc(subDatas)

}

trait LeafPile extends CommonPile {
  self =>

  def leafZero: List[DataPile] = {
    DataPile.fromPile(self, fShape.zero :: Nil)._1 :: Nil
  }

}

class LeafPileImpl[PT, DT](
    override val pathPile: PT,
    override val fShape: PileShape[PT, DT]
) extends LeafPile {
  override type PathType = PT
  override type DataType = DT
}

object Pile {
  def apply[D](paths: AtomicPathImpl[D]): LeafPileImpl[AtomicPathImpl[D], AtomicValueImpl[D]] = {
    val shape = PileShape.fpathPileShape[D]
    new LeafPileImpl(paths, shape)
  }
}