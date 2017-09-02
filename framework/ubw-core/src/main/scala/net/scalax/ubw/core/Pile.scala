package net.scalax.fsn.core

import net.scalax.fsn.core.ListUtils.WeightData
import net.scalax.ubw.core.PileFilter
import scala.language.higherKinds

sealed abstract trait Pile {
  self =>
  type DataType

  def dataLengthSum: Int

  def deepZero: List[AtomicValue]

  def subsCommonPile: List[LeafPile]

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

trait PileList extends Pile {
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

  override def subsCommonPile: List[LeafPile] = {
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

  def encodePiles(piles: PileType): List[CommonPile]
  def decodePiles(piles: List[CommonPile]): PileType
  def decodePileData(datas: List[Any]): DataType
  def encodePileData(data: DataType): List[Any]

}

class PileListImpl[PT, DT](
    override val pileEntity: PT,
    encoder: PT => List[CommonPile],
    decoder: List[CommonPile] => PT,
    dataDecoder: List[Any] => DT,
    dataEncoder: DT => List[Any]
) extends PileList {
  override type PileType = PT
  override type DataType = DT

  override def encodePiles(piles: PT): List[CommonPile] = encoder(piles)
  override def decodePiles(piles: List[CommonPile]): PileType = decoder(piles)
  override def decodePileData(data: List[Any]): DT = dataDecoder(data)
  override def encodePileData(data: DataType): List[Any] = dataEncoder(data)
}

abstract trait CommonPile extends Pile {
  self =>

  type PathType
  override type DataType

  val pathPile: PathType
  val fShape: PileShape[PathType, DataType]

  override def selfPaths: List[AtomicPath] = {
    self.fShape.encodeColumn(self.pathPile)
  }

}

trait BranchPile extends CommonPile {
  self =>

  val subs: Pile
  def dataFromSub(subDatas: Any): DataType

  override def dataLengthSum: Int = {
    self.subs.dataLengthSum
  }

  override def deepZero: List[AtomicValue] = {
    self.subs.deepZero
  }

  override def subsCommonPile: List[LeafPile] = {
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

  override def dataLengthSum: Int = {
    self.fShape.dataLength
  }

  override def deepZero: List[AtomicValue] = {
    self.fShape.encodeData(self.fShape.zero)
  }

  override def subsCommonPile: List[LeafPile] = {
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

class LeafPileImpl[PT, DT](
    override val pathPile: PT,
    override val fShape: PileShape[PT, DT]
) extends LeafPile {
  override type PathType = PT
  override type DataType = DT
}

object Pile {

  case class TransPileWrap(root: Pile, drops: List[Pile])
  type TransResult[T] = Either[AtomicException, T]

  def apply[D](paths: AtomicPathImpl[D]): LeafPileImpl[AtomicPathImpl[D], AtomicValueImpl[D]] = {
    val shape = PileShape.fpathPileShape[D]
    new LeafPileImpl(paths, shape)
  }

  def weightDataListFromSubWithFilter[U, F[_]](atomicDatas: List[WeightData[(AtomicValue, AtomicPath)]], filter: PileFilter[U, F]): (F[List[WeightData[AtomicValue]]], F[List[U]]) = {
    val atomicValues = atomicDatas.map { s =>
      val (newAtomnicValues, filterResults) = s.data.map {
        case (atomicValue, atomicPath) =>
          val transform = filter.transform.apply(atomicPath.asInstanceOf[AtomicPathImpl[atomicPath.DataType]])
          transform.gen match {
            case Left(_) =>
              filter.monad.pure(atomicValue: AtomicValue) -> List.empty[F[U]]
            case Right(query) =>
              val result = transform.apply(query, atomicValue.asInstanceOf[AtomicValueImpl[transform.path.DataType]])
              val (newValue, filterResult) = filter.unzip(result)
              filter.monad.map(newValue)({ s => s: AtomicValue }) -> List(filterResult)
          }
      }.unzip
      val AtomicValues = filter.listTraverse(newAtomnicValues)
      filter.monad.map(AtomicValues)({ values => WeightData(values, s.weight) }) -> filterResults.flatten
    }
    val (newAtomicValues, filterResult) = atomicValues.unzip
    filter.listTraverse(newAtomicValues) -> filter.listTraverse(filterResult.flatten)
  }

  def genTreeTailCall[U](pathGen: AtomicPath => QueryTranform[U], oldPile: Pile, newPile: Pile): TransResult[TransPileWrap] = {
    oldPile -> newPile match {
      case (commonPile: CommonPile, leafPile: LeafPile) =>
        val transforms = leafPile.fShape.encodeColumn(leafPile.pathPile).map(pathGen)
        if (transforms.forall(_.gen.isRight)) {
          Right(TransPileWrap(newPile, List(commonPile)))
        } else {
          Left(AtomicException(transforms.map(_.gen).collect { case Left(AtomicException(s)) => s }.flatten))
        }

      case (oldPile: BranchPile, newPile: BranchPile) =>
        genTreeTailCall(pathGen, oldPile.subs, newPile.subs) match {
          case Left(_) =>
            genTreeTailCall(pathGen, oldPile, new LeafPileImpl(
              newPile.pathPile, newPile.fShape
            ))
          case Right(TransPileWrap(newSubResultPile, pileList)) =>
            Right(TransPileWrap(new BranchPileImpl(
              newPile.pathPile,
              newPile.fShape,
              newSubResultPile,
              newPile.dataFromSub _
            ), pileList))
        }

      case (oldPile: PileList, newPile: PileList) =>
        val newPiles = newPile.encodePiles(newPile.pileEntity)
        val oldPiles = oldPile.encodePiles(oldPile.pileEntity)
        val listResult = oldPiles.zip(newPiles).map {
          case (oldP, newP) =>
            genTreeTailCall(pathGen, oldP, newP)
        }
        val isSuccess = listResult.forall(_.isRight)
        if (isSuccess) {
          val (newPiles, newPileList) = listResult.map {
            case Right(TransPileWrap(root, drops)) => root -> drops
            case _ => throw new IllegalArgumentException("不可识别的输入")
          }.unzip
          Right(TransPileWrap(new PileListImpl(
            newPile.decodePiles(newPiles.map(_.asInstanceOf[CommonPile])),
            newPile.encodePiles _,
            newPile.decodePiles _,
            newPile.decodePileData _,
            newPile.encodePileData _
          ), newPileList.flatten))
        } else {
          Left(listResult.collect { case Left(ex) => ex }.reduce((a1, a2) =>
            AtomicException(a1.typeTags ::: a2.typeTags)))
        }
      case _ => throw new IllegalArgumentException("不可识别的输入")
    }
  }

  def genTree[U](pathGen: AtomicPath => QueryTranform[U], pile: Pile): TransResult[TransPileWrap] = {
    genTreeTailCall(pathGen, pile, pile)
  }

  def transformTreeList[U, T](pathGen: AtomicPath => QueryTranform[U])(columnGen: List[U] => T): PileSyntax.PileGen[T] = new PileSyntax.PileGen[T] {
    override def gen(prePiles: List[Pile]) = {
      transformTreeListWithFilter(pathGen, PileFilter.empty)(columnGen, { s => s }).gen(prePiles) match {
        case Left(e) => Left(e)
        case Right(result) => Right(PileSyntax.PilePip(result.piles, { atomics => result.atomicValues(atomics)._1 }))
      }
      //防止定义 Pile 时最后一步使用了混合后不能识别最后一层 path
      /*val piles = prePiles

      val calculatePiles = piles.map { s =>
        genTree(pathGen, s)
      }.foldLeft(Right(Nil): TransResult[List[TransPileWrap]]) {
        (append, eitherResult) =>
          (append -> eitherResult) match {
            case (Left(s), Left(t)) =>
              Left(AtomicException(s.typeTags ::: t.typeTags))
            case (Left(s), Right(_)) =>
              Left(AtomicException(s.typeTags))
            case (Right(_), Left(s)) =>
              Left(AtomicException(s.typeTags))
            case (Right(s), Right(t)) =>
              Right(t :: s)
          }
      }.right.map(_.reverse)
      calculatePiles.right.map { pileList =>
        val (newPile, summaryPiles) = pileList.map(s => s.root -> s.drops).unzip
        PileSyntax.PilePip(newPile, { anyList: List[AtomicValue] =>
          columnGen(ListUtils.splitList(anyList, summaryPiles.map(_.map(_.dataLengthSum).sum): _*)
            .zip(summaryPiles)
            .flatMap {
              case (subList, subPiles) =>
                ListUtils.splitList(subList, subPiles.map(_.dataLengthSum): _*).zip(subPiles).flatMap {
                  case (eachList, eachPiles) =>
                    eachPiles.selfPaths.map(s => pathGen(s)).zip(eachPiles.dataListFromSubList(eachList)).map {
                      case (tranform, data) =>
                        tranform.apply(tranform.gen.right.get, data.asInstanceOf[AtomicValueImpl[tranform.path.DataType]])
                    }
                }
            })
        })
      }*/
    }

  }

  def transformTreeListWithFilter[U, T, E, F[_], G](pathGen: AtomicPath => QueryTranform[U], filter: PileFilter[E, F])(columnGen: F[List[U]] => T, filterGen: F[List[E]] => G): PileSyntax.PileGen[(T, G)] = new PileSyntax.PileGen[(T, G)] {
    override def gen(prePiles: List[Pile]) = {
      //防止定义 Pile 时最后一步使用了混合后不能识别最后一层 path
      val piles = prePiles //.flatMap(eachPile => eachPile.genPiles)

      val calculatePiles = piles.map { s =>
        genTree(pathGen, s)
      }.foldLeft(Right(Nil): TransResult[List[TransPileWrap]]) {
        (append, eitherResult) =>
          (append -> eitherResult) match {
            case (Left(s), Left(t)) =>
              Left(AtomicException(s.typeTags ::: t.typeTags))
            case (Left(s), Right(_)) =>
              Left(AtomicException(s.typeTags))
            case (Right(_), Left(s)) =>
              Left(AtomicException(s.typeTags))
            case (Right(s), Right(t)) =>
              Right(t :: s)
          }
      }.right.map(_.reverse)
      calculatePiles.right.map { pileList =>
        val (newPiles, summaryPiles) = pileList.map(s => s.root -> s.drops).unzip
        PileSyntax.PilePip(newPiles, { anyList: List[AtomicValue] =>
          val (valueGens, filterResult) = ListUtils.splitList(anyList, summaryPiles.map(_.map(_.dataLengthSum).sum): _*)
            .zip(summaryPiles)
            .flatMap {
              case (subList, subPiles) =>
                ListUtils.splitList(subList, subPiles.map(_.dataLengthSum): _*).zip(subPiles).map {
                  case (eachList, eachPiles) =>
                    val (newDataList, filterResults) = eachPiles.dataListFromSubListWithFilter(eachList, filter)
                    filter.monad.map(newDataList) { dataList =>
                      eachPiles.selfPaths.map(s => pathGen(s)).zip(dataList /*eachPiles.dataListFromSubList(eachList)*/ ).map {
                        case (tranform, data) =>
                          tranform.apply(tranform.gen.right.get, data.asInstanceOf[AtomicValueImpl[tranform.path.DataType]])
                      }
                    } -> filterResults
                }
            }.unzip
          columnGen(filter.monad.map(filter.listTraverse(valueGens))(_.flatten)) -> filterGen(filter.monad.map(filter.listTraverse(filterResult)) { _.flatten })
        })
      }

    }

  }

  def transformOf[U, T](pathGen: AtomicPath => QueryTranform[U])(columnGen: List[U] => T): List[AtomicPath] => TransResult[List[AtomicValue] => T] = {
    (initPaths: List[AtomicPath]) =>
      {
        initPaths.map(pathGen).zipWithIndex.foldLeft(Right { _: List[AtomicValue] => Nil }: Either[AtomicException, List[AtomicValue] => List[U]]) {
          case (convert, (queryTranform, index)) =>
            (convert -> queryTranform.gen) match {
              case (Left(s), Left(t)) =>
                Left(AtomicException(s.typeTags ::: t.typeTags))
              case (Left(s), Right(_)) =>
                Left(AtomicException(s.typeTags))
              case (Right(_), Left(s)) =>
                Left(AtomicException(s.typeTags))
              case (Right(s), Right(t)) =>
                Right { list: List[AtomicValue] =>
                  queryTranform.apply(t, list(index).asInstanceOf[AtomicValueImpl[queryTranform.path.DataType]]) :: s(list)
                }
            }
        }.right.map { s => (t: List[AtomicValue]) => {
          columnGen(s(t))
        }
        }
      }
  }

  /*def genTreeTailCallWithoutData[U](pathGen: AtomicPath => QueryTranformWithOutData[U], oldPile: Pile, newPile: Pile): Either[AtomicException, (Pile, Pile, List[Pile])] = {
    /*if (newPile.subs.isEmpty) {
      val transforms = newPile.paths.map(pathGen)
      if (transforms.forall(_.gen.isRight)) {
        Right(oldPile, newPile, List(oldPile))
      } else {
        Left(AtomicException(transforms.map(_.gen).collect { case Left(AtomicException(s)) => s }.flatten))
      }
    } else {*/
    /*if (newPile.subs.isEmpty) {
      val transforms = newPile.paths.map(pathGen)
      if (transforms.forall(_.gen.isRight)) {
        Right(oldPile, newPile, List(oldPile))
      } else {
        Left(AtomicException(transforms.map(_.gen).collect { case Left(AtomicException(s)) => s }.flatten))
      }
    } else {
      val newSubs = oldPile.subs.flatMap(_.genPiles).zip(newPile.subs.flatMap(_.genPiles)).map { case (eachOldPile, eachNewPile) => genTreeTailCallWithoutData(pathGen, eachOldPile, eachNewPile) }
      //val newSubs = oldPile.subs.zip(newPile.subs).map { case (eachOldPile, eachNewPile) => genTreeTailCallWithoutData(pathGen, eachOldPile, eachNewPile) }
      if (newSubs.forall(_.isRight)) {
        val (_, newSubTree, successNodes) = newSubs.map(_.right.get).unzip3
        val newNode = new PileImpl(newPile.pathPile, newPile.fShape, newPile.dataFromSub, newSubTree) {
          self =>
          //TODO
          override def genPiles = List(self) //throw new Exception("不应该使用")
        } /*()*/
        Right(oldPile, newNode, successNodes.flatten)
      } else {
        genTreeTailCallWithoutData(pathGen, oldPile, new PileImpl(newPile.pathPile, newPile.fShape, (_: List[Any]) => newPile.fShape.zero, Nil) {
          self =>
          //TODO
          override def genPiles = List(self) //throw new Exception("不应该使用")
        })
      }
    }*/
    ???
  }

  def genTreeWithoutData[U](pathGen: AtomicPath => QueryTranformWithOutData[U], pile: Pile): Either[AtomicException, (Pile, List[Pile])] = {
    genTreeTailCallWithoutData(pathGen, pile, pile).right.map { case (oldPile, newPile, piles) => newPile -> piles }
  }

  def transformTreeListWithoutData[U, T](pathGen: AtomicPath => QueryTranformWithOutData[U])(columnGen: List[U] => T): PileSyntaxWithoutData.PileGen[T] = {
    prePiles: List[Pile] =>
      //防止定义 Pile 时最后一步使用了混合后不能识别最后一层 path
      /*val piles = prePiles.flatMap(eachPile => eachPile.genPiles)
      val calculatePiles = piles.map { s =>
        genTreeWithoutData(pathGen, s)
      }.foldLeft(Right(Nil): Either[AtomicException, List[(Pile, List[Pile])]]) {
        (append, eitherResult) =>
          (append -> eitherResult) match {
            case (Left(s), Left(t)) =>
              Left(AtomicException(s.typeTags ::: t.typeTags))
            case (Left(s), Right(_)) =>
              Left(AtomicException(s.typeTags))
            case (Right(_), Left(s)) =>
              Left(AtomicException(s.typeTags))
            case (Right(s), Right(t)) =>
              Right(t :: s)
          }
      }.right.map(_.reverse)
      calculatePiles.right.map { pileList =>
        val (newPile, summaryPiles) = pileList.unzip
        newPile -> {
          columnGen(summaryPiles.map { subPiles =>
            subPiles.map { eachPiles =>
              eachPiles.paths.map(s => pathGen(s)).map { tranform =>
                tranform.apply(tranform.gen.right.get)
              }
            }
          }.flatten.flatten)
        }
      }*/
      ???
  }*/

  /*def transformOf[U, T](pathGen: AtomicPath => QueryTranform[U])(columnGen: List[U] => T): List[AtomicPath] => Either[AtomicException, List[AtomicValue] => T] = {
     (initPaths: List[AtomicPath]) =>
       ???
     {
       initPaths.map(pathGen).zipWithIndex.foldLeft(Right { _: List[AtomicValue] => Nil }: Either[AtomicException, List[AtomicValue] => List[U]]) {
         case (convert, (queryTranform, index)) =>
           (convert -> queryTranform.gen) match {
             case (Left(s), Left(t)) =>
               Left(AtomicException(s.typeTags ::: t.typeTags))
             case (Left(s), Right(_)) =>
               Left(AtomicException(s.typeTags))
             case (Right(_), Left(s)) =>
               Left(AtomicException(s.typeTags))
             case (Right(s), Right(t)) =>
               Right { list: List[AtomicValue] =>
                 queryTranform.apply(t, list(index).asInstanceOf[AtomicValueImpl[queryTranform.path.DataType]]) :: s(list)
               }
           }
       }.right.map { s => (t: List[AtomicValue]) => {
         columnGen(s(t))
       }
       }
     }
   }*/
}