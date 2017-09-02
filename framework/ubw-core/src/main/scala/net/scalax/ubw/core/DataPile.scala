package net.scalax.fsn.core

import net.scalax.fsn.core.ListUtils.WeightData
import net.scalax.ubw.core.PileFilter
import scala.language.higherKinds

sealed abstract trait DataPile {
  self =>
  type DataType

  def dataLengthSum: Int

  def deepZero: List[AtomicValue]

  def subsCommonPile: List[LeafDataPile]

  def selfPaths: List[AtomicPath]

  def dataListFromSubList(atomicDatas: List[AtomicValue]): List[AtomicValue] = {
    val leave = subsCommonPile
    val atomicValueList = ListUtils.splitList(atomicDatas, leave.map(_.dataLengthSum): _*)
    val weightData = leave.zip(atomicValueList).map { case (eachPile, values) => WeightData(values, eachPile.dataLengthSum) }
    weightDataListFromSubList(weightData).flatMap(_.data)
  }

  def dataListFromSubListWithFilter[U, F[_]](atomicDatas: List[AtomicValue], filter: PileFilter[U, F]): (F[List[AtomicValue]], F[List[U]]) = {
    val leave = subsCommonPile
    val atomicValueList = ListUtils.splitList(atomicDatas, leave.map(_.dataLengthSum): _*)
    val weightData = leave.zip(atomicValueList).map { case (eachPile, values) => WeightData(values, eachPile.dataLengthSum) }
    val (weightValues, filterResults) = dataListFromSubListWithFilter1(weightData, filter)
    filter.monad.map(weightValues) { values => values.flatMap(_.data) } -> filterResults
  }

  def dataListFromSubListWithFilter1[U, F[_]](atomicDatas: List[WeightData[AtomicValue]], filter: PileFilter[U, F]): (F[List[WeightData[AtomicValue]]], F[List[U]])

  def weightDataListFromSubList(atomicDatas: List[WeightData[AtomicValue]]): List[WeightData[AtomicValue]]

}

trait DataPileList extends DataPile {
  self =>

  type PileType
  override type DataType

  override def dataLengthSum: Int = {
    self.encodePiles(self.pileEntity).map(_.dataLengthSum).sum
  }

  override def deepZero: List[AtomicValue] = {
    self.encodePiles(self.pileEntity).flatMap(_.deepZero)
  }

  override def selfPaths: List[AtomicPath] = {
    self.encodePiles(self.pileEntity).flatMap(_.selfPaths)
  }

  override def subsCommonPile: List[LeafDataPile] = {
    self.encodePiles(self.pileEntity).flatMap(_.subsCommonPile)
  }

  override def weightDataListFromSubList(atomicDatas: List[WeightData[AtomicValue]]): List[WeightData[AtomicValue]] = {
    //如果是 pileList，直接分组再递归调用
    val piles = self.encodePiles(self.pileEntity)
    val datas = ListUtils.splitWithWeight(atomicDatas, piles.map(_.dataLengthSum): _*)
    val pileWithData = if (piles.size == datas.size) {
      piles.zip(datas)
    } else {
      throw new Exception("pile 与数据长度不匹配")
    }
    pileWithData.flatMap {
      case (eachPile, eachData) =>
        eachPile.weightDataListFromSubList(eachData)
    }
  }

  override def dataListFromSubListWithFilter1[U, F[_]](atomicDatas: List[WeightData[AtomicValue]], filter: PileFilter[U, F]): (F[List[WeightData[AtomicValue]]], F[List[U]]) = {
    val piles = self.encodePiles(self.pileEntity)

    val dataWithPiles = ListUtils.splitWithWeight(atomicDatas, piles.map(_.dataLengthSum): _*).zip(piles)
    val pileWithData = dataWithPiles.map {
      case (weightData, pile) =>
        pile.dataListFromSubListWithFilter1(weightData, filter)
    }.unzip
    filter.monad.map(filter.listTraverse(pileWithData._1)) { s => s.flatten } -> filter.monad.map(filter.listTraverse(pileWithData._2))(_.flatten)
  }

  val pileEntity: PileType

  def encodePiles(piles: PileType): List[CommonDataPile]
  def decodePiles(piles: List[CommonDataPile]): PileType
  def decodePileData(datas: List[Any]): DataType

}

class DataPileListImpl[PT, DT](
    override val pileEntity: PT,
    encoder: PT => List[CommonDataPile],
    decoder: List[CommonDataPile] => PT,
    dataDecoder: List[Any] => DT
) extends DataPileList {
  override type PileType = PT
  override type DataType = DT

  override def encodePiles(piles: PT): List[CommonDataPile] = encoder(piles)
  override def decodePiles(piles: List[CommonDataPile]): PileType = decoder(piles)
  override def decodePileData(datas: List[Any]): DT = dataDecoder(datas)
}

abstract trait CommonDataPile extends DataPile {
  self =>

  type PathType
  override type DataType

  val pathPile: PathType
  val fShape: PileShape[PathType, DataType]

  override def selfPaths: List[AtomicPath] = {
    self.fShape.encodeColumn(self.pathPile)
  }

}

trait BranchDataPile extends CommonDataPile {
  self =>

  val subs: DataPile
  def dataFromSub(subDatas: Any): DataType

  override def dataLengthSum: Int = {
    self.subs.dataLengthSum
  }

  override def deepZero: List[AtomicValue] = {
    self.subs.deepZero
  }

  override def subsCommonPile: List[LeafDataPile] = {
    self.subs.subsCommonPile
  }

  override def dataListFromSubListWithFilter1[U, F[_]](atomicDatas: List[WeightData[AtomicValue]], filter: PileFilter[U, F]): (F[List[WeightData[AtomicValue]]], F[List[U]]) = {
    val subPiles = self.subs
    val (subDataF, filterResult) = subPiles.dataListFromSubListWithFilter1(atomicDatas, filter)
    val result = filter.monad.map(subDataF) { subData =>
      subPiles match {
        case sp: CommonPile =>
          if (subData.size != 1) {
            throw new Exception("CommonPile 的权重数据长度必须为 1")
          }
          val subPileData = sp.fShape.decodeData(subData.head.data)
          val currentPileData = self.dataFromSub(subPileData)
          val resultDataList = self.fShape.encodeData(currentPileData)
          Pile.weightDataListFromSubWithFilter(List(WeightData(resultDataList.zip(self.selfPaths), self.dataLengthSum)), filter)
        case sp: PileList =>
          val piles = sp.encodePiles(sp.pileEntity)
          if (subData.size != piles.size) {
            throw new Exception("PileList 的权重数据长度和 pile 数量不一致")
          }
          val subDataList = ListUtils.splitWithWeight(subData, piles.map(_.dataLengthSum): _*)
          val pileWithData = piles.zip(subDataList)
          val currentPileData = sp.decodePileData {
            pileWithData.map {
              case (eachPile, subData) =>
                if (subData.size != 1) {
                  throw new Exception("CommonPile 的权重数据长度必须为 1")
                }
                eachPile.fShape.decodeData(subData.head.data)
            }
          }
          val resultDataList = self.fShape.encodeData(self.dataFromSub(currentPileData))
          //List(WeightData(resultDataList, self.dataLengthSum))
          Pile.weightDataListFromSubWithFilter(List(WeightData(resultDataList.zip(self.selfPaths), self.dataLengthSum)), filter)
      }

    }

    filter.monad.flatMap(result) { s => s._1 } -> filter.monad.flatMap(result) { s =>
      filter.monad.flatMap(s._2) { t => filter.monad.map(filterResult) { result => t ::: result } }
    }

  }

  override def weightDataListFromSubList(atomicDatas: List[WeightData[AtomicValue]]): List[WeightData[AtomicValue]] = {
    val subPiles = self.subs
    val subData = subPiles.weightDataListFromSubList(atomicDatas)
    subPiles match {
      case sp: CommonPile =>
        if (subData.size != 1) {
          throw new Exception("CommonPile 的权重数据长度必须为 1")
        }
        val subPileData = sp.fShape.decodeData(subData.head.data)
        val currentPileData = self.dataFromSub(subPileData)
        val resultDataList = self.fShape.encodeData(currentPileData)
        List(WeightData(resultDataList, self.dataLengthSum))
      case sp: PileList =>
        val piles = sp.encodePiles(sp.pileEntity)
        if (subData.size != piles.size) {
          throw new Exception("PileList 的权重数据长度和 pile 数量不一致")
        }
        val subDataList = ListUtils.splitWithWeight(subData, piles.map(_.dataLengthSum): _*)
        val pileWithData = piles.zip(subDataList)
        val currentPileData = sp.decodePileData {
          pileWithData.map {
            case (eachPile, subData) =>
              if (subData.size != 1) {
                throw new Exception("CommonPile 的权重数据长度必须为 1")
              }
              eachPile.fShape.decodeData(subData.head.data)
          }
        }
        val resultDataList = self.fShape.encodeData(self.dataFromSub(currentPileData))
        List(WeightData(resultDataList, self.dataLengthSum))
    }
  }

}

class BranchDataPileImpl[PT, DT](
    override val pathPile: PT,
    override val fShape: PileShape[PT, DT],
    override val subs: DataPile,
    dataFromSubFunc: Any => DT
) extends BranchDataPile {
  override type PathType = PT
  override type DataType = DT

  override def dataFromSub(subDatas: Any): DataType = dataFromSubFunc(subDatas)

}

trait LeafDataPile extends CommonDataPile {
  self =>

  override def dataLengthSum: Int = {
    self.fShape.dataLength
  }

  override def deepZero: List[AtomicValue] = {
    self.fShape.encodeData(self.fShape.zero)
  }

  override def subsCommonPile: List[LeafDataPile] = {
    List(self)
  }

  override def weightDataListFromSubList(atomicDatas: List[WeightData[AtomicValue]]): List[WeightData[AtomicValue]] = {
    atomicDatas
  }

  override def dataListFromSubListWithFilter1[U, F[_]](atomicDatas: List[WeightData[AtomicValue]], filter: PileFilter[U, F]): (F[List[WeightData[AtomicValue]]], F[List[U]]) = {
    val leave = subsCommonPile
    if (atomicDatas.size != 1) {
      throw new Exception("LeafPile 的数据束数量只能为 1")
    }
    if (atomicDatas.head.data.size != selfPaths.size) {
      throw new Exception("LeafPile 的 AtomicValue 数量和 AtomicPath 数量不匹配")
    }
    val singleWeightData = atomicDatas.head.copy(data = atomicDatas.head.data.zip(selfPaths))
    //val atomicValueList = ListUtils.splitList(atomicDatas.zip(selfPaths), leave.map(_.dataLengthSum): _*)
    //val weightData = leave.zip(atomicValueList).map { case (eachPile, values) => WeightData(values, eachPile.dataLengthSum) }
    val (weightValues, filterResults) = Pile.weightDataListFromSubWithFilter(List(singleWeightData), filter)
    weightValues -> filterResults
  }

}

class LeafDataPileImpl[PT, DT](
    override val pathPile: PT,
    override val fShape: PileShape[PT, DT]
) extends FLeafPile {
  override type PathType = PT
  override type DataType = DT
}

object DataPile {

}