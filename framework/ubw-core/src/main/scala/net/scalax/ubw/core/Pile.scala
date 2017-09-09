package net.scalax.fsn.core

sealed abstract trait Pile {
  self =>
  type DataType

  def leafZero: List[AtomicValue]
  def leafZeroDataPiles: List[DataPile]

}

trait PileList extends Pile {
  self =>

  type PileType
  override type DataType

  def leafZero: List[AtomicValue] = {
    encodePiles.map(_.leafZero).flatten
  }
  def leafZeroDataPiles: List[DataPile] = {
    encodePiles.map(_.leafZeroDataPiles).flatten
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

  def leafZero: List[AtomicValue] = {
    subs.leafZero
  }
  def leafZeroDataPiles: List[DataPile] = {
    subs.leafZeroDataPiles
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

  def leafZero: List[AtomicValue] = {
    //DataPile.fromPile(self, fShape.zero :: Nil)._1 :: Nil
    fShape.encodeData(fShape.zero)
  }
  def leafZeroDataPiles: List[DataPile] = {
    List(
      new LeafDataPileImpl(
        pathPile = self.pathPile,
        data = self.fShape.zero,
        fShape = self.fShape
      )
    )
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