package net.scalax.fsn.core

import net.scalax.fsn.core.ListUtils.WeightData

sealed abstract trait FPile {
  self =>
  type DataType

  def dataLengthSum: Int

  def deepZero: List[FAtomicValue]

  def subsCommonPile: List[FLeafPile]

  def selfPaths: List[FAtomicPath]

  def dataListFromSubList(atomicDatas: List[FAtomicValue]): List[FAtomicValue] = {
    val leave = subsCommonPile
    val atomicValueList = ListUtils.splitList(atomicDatas, leave.map(_.dataLengthSum): _*)
    val weightData = leave.zip(atomicValueList).map { case (eachPile, values) => WeightData(values, eachPile.dataLengthSum) }
    weightDataListFromSubList(weightData).flatMap(_.data)
  }

  def weightDataListFromSubList(atomicDatas: List[WeightData[FAtomicValue]]): List[WeightData[FAtomicValue]]
  /*def weightDataListFromSubList(atomicDatas: List[WeightData[FAtomicValue]]): List[WeightData[FAtomicValue]] = {
    self match {
      case s: FPileList =>
        //如果是 pileList，直接分组再递归调用
        val piles = s.encodePiles(s.pileEntity)
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
      case _: FLeafPile =>
        atomicDatas
      case s: FBranchPile =>
        val subPiles = s.subs
        val subData = subPiles.weightDataListFromSubList(atomicDatas)
        subPiles match {
          case sp: FCommonPile =>
            if (subData.size != 1) {
              throw new Exception("FCommonPile 的权重数据长度必须为 1")
            }
            val subPileData = sp.fShape.decodeData(subData.head.data)
            val currentPileData = s.dataFromSub(subPileData)
            val resultDataList = s.fShape.encodeData(currentPileData)
            List(WeightData(resultDataList, s.dataLengthSum))
          case sp: FPileList =>
            val piles = sp.encodePiles(sp.pileEntity)
            if (subData.size != piles.size) {
              throw new Exception("FPileList 的权重数据长度和 pile 数量不一致")
            }
            val subDataList = ListUtils.splitWithWeight(subData, piles.map(_.dataLengthSum): _*)
            val pileWithData = piles.zip(subDataList)
            val currentPileData = sp.decodePileData {
              pileWithData.map {
                case (eachPile, subData) =>
                  if (subData.size != 1) {
                    throw new Exception("FCommonPile 的权重数据长度必须为 1")
                  }
                  eachPile.fShape.decodeData(subData.head.data)
              }
            }
            val resultDataList = s.fShape.encodeData(s.dataFromSub(currentPileData))
            List(WeightData(resultDataList, s.dataLengthSum))
        }
    }
  }*/
}

trait FPileList extends FPile {
  self =>

  type PileType
  override type DataType

  override def dataLengthSum: Int = {
    self.encodePiles(self.pileEntity).map(_.dataLengthSum).sum
  }

  override def deepZero: List[FAtomicValue] = {
    self.encodePiles(self.pileEntity).flatMap(_.deepZero)
  }

  override def selfPaths: List[FAtomicPath] = {
    self.encodePiles(self.pileEntity).flatMap(_.selfPaths)
  }

  override def subsCommonPile: List[FLeafPile] = {
    self.encodePiles(self.pileEntity).flatMap(_.subsCommonPile)
  }

  override def weightDataListFromSubList(atomicDatas: List[WeightData[FAtomicValue]]): List[WeightData[FAtomicValue]] = {
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

  val pileEntity: PileType

  def encodePiles(piles: PileType): List[FCommonPile]
  def decodePiles(piles: List[FCommonPile]): PileType
  def decodePileData(datas: List[Any]): DataType

}

class FPileListImpl[PT, DT](
    override val pileEntity: PT,
    encoder: PT => List[FCommonPile],
    decoder: List[FCommonPile] => PT,
    dataDecoder: List[Any] => DT
) extends FPileList {
  override type PileType = PT
  override type DataType = DT

  override def encodePiles(piles: PT): List[FCommonPile] = encoder(piles)
  override def decodePiles(piles: List[FCommonPile]): PileType = decoder(piles)
  override def decodePileData(datas: List[Any]): DT = dataDecoder(datas)
}

abstract trait FCommonPile extends FPile {
  self =>

  type PathType
  override type DataType

  val pathPile: PathType
  val fShape: FsnShape[PathType, DataType]

  override def selfPaths: List[FAtomicPath] = {
    self.fShape.encodeColumn(self.pathPile)
  }

}

trait FBranchPile extends FCommonPile {
  self =>

  val subs: FPile
  def dataFromSub(subDatas: Any): DataType

  override def dataLengthSum: Int = {
    self.subs.dataLengthSum
  }

  override def deepZero: List[FAtomicValue] = {
    self.subs.deepZero
  }

  override def subsCommonPile: List[FLeafPile] = {
    self.subs.subsCommonPile
  }

  override def weightDataListFromSubList(atomicDatas: List[WeightData[FAtomicValue]]): List[WeightData[FAtomicValue]] = {
    val subPiles = self.subs
    val subData = subPiles.weightDataListFromSubList(atomicDatas)
    subPiles match {
      case sp: FCommonPile =>
        if (subData.size != 1) {
          throw new Exception("FCommonPile 的权重数据长度必须为 1")
        }
        val subPileData = sp.fShape.decodeData(subData.head.data)
        val currentPileData = self.dataFromSub(subPileData)
        val resultDataList = self.fShape.encodeData(currentPileData)
        List(WeightData(resultDataList, self.dataLengthSum))
      case sp: FPileList =>
        val piles = sp.encodePiles(sp.pileEntity)
        if (subData.size != piles.size) {
          throw new Exception("FPileList 的权重数据长度和 pile 数量不一致")
        }
        val subDataList = ListUtils.splitWithWeight(subData, piles.map(_.dataLengthSum): _*)
        val pileWithData = piles.zip(subDataList)
        val currentPileData = sp.decodePileData {
          pileWithData.map {
            case (eachPile, subData) =>
              if (subData.size != 1) {
                throw new Exception("FCommonPile 的权重数据长度必须为 1")
              }
              eachPile.fShape.decodeData(subData.head.data)
          }
        }
        val resultDataList = self.fShape.encodeData(self.dataFromSub(currentPileData))
        List(WeightData(resultDataList, self.dataLengthSum))
    }
  }

}

class FBranchPileImpl[PT, DT](
    override val pathPile: PT,
    override val fShape: FsnShape[PT, DT],
    override val subs: FPile,
    dataFromSubFunc: Any => DT
) extends FBranchPile {
  override type PathType = PT
  override type DataType = DT

  override def dataFromSub(subDatas: Any): DataType = dataFromSubFunc(subDatas)

}

trait FLeafPile extends FCommonPile {
  self =>

  override def dataLengthSum: Int = {
    self.fShape.dataLength
  }

  override def deepZero: List[FAtomicValue] = {
    self.fShape.encodeData(self.fShape.zero)
  }

  override def subsCommonPile: List[FLeafPile] = {
    List(self)
  }

  override def weightDataListFromSubList(atomicDatas: List[WeightData[FAtomicValue]]): List[WeightData[FAtomicValue]] = {
    atomicDatas
  }

}

class FLeafPileImpl[PT, DT](
    override val pathPile: PT,
    override val fShape: FsnShape[PT, DT]
) extends FLeafPile {
  override type PathType = PT
  override type DataType = DT
}

object FPile {

  case class TransPileWrap(root: FPile, drops: List[FPile])
  type TransResult[T] = Either[FAtomicException, T]

  def apply[D](paths: FAtomicPathImpl[D]): FLeafPileImpl[FAtomicPathImpl[D], FAtomicValueImpl[D]] = {
    val shape = FsnShape.fpathFsnShape[D]
    new FLeafPileImpl(paths, shape)
  }

  def genTreeTailCall[U](pathGen: FAtomicPath => FQueryTranform[U], oldPile: FPile, newPile: FPile): TransResult[TransPileWrap] = {
    oldPile -> newPile match {
      case (commonPile: FCommonPile, leafPile: FLeafPile) =>
        val transforms = leafPile.fShape.encodeColumn(leafPile.pathPile).map(pathGen)
        if (transforms.forall(_.gen.isRight)) {
          Right(TransPileWrap(newPile, List(commonPile)))
        } else {
          Left(FAtomicException(transforms.map(_.gen).collect { case Left(FAtomicException(s)) => s }.flatten))
        }

      case (oldPile: FBranchPile, newPile: FBranchPile) =>
        genTreeTailCall(pathGen, oldPile.subs, newPile.subs) match {
          case Left(_) =>
            genTreeTailCall(pathGen, oldPile, new FLeafPileImpl(
              newPile.pathPile, newPile.fShape
            ))
          case Right(TransPileWrap(newSubResultPile, pileList)) =>
            Right(TransPileWrap(new FBranchPileImpl(
              newPile.pathPile,
              newPile.fShape,
              newSubResultPile,
              newPile.dataFromSub _
            ), pileList))
        }

      case (oldPile: FPileList, newPile: FPileList) =>
        val newPiles = newPile.encodePiles(newPile.pileEntity)
        val oldPiles = oldPile.encodePiles(oldPile.pileEntity)
        val listResult = oldPiles.zip(newPiles).map {
          case (oldP, newP) =>
            genTreeTailCall(pathGen, oldP, newP)
        }
        val isSuccess = listResult.forall(_.isRight)
        if (isSuccess) {
          val (newPiles, newPileList) = listResult.map { case Right(TransPileWrap(root, drops)) => root -> drops }.unzip
          Right(TransPileWrap(new FPileListImpl(
            newPile.decodePiles(newPiles.map(_.asInstanceOf[FCommonPile])),
            newPile.encodePiles _,
            newPile.decodePiles _,
            newPile.decodePileData _
          ), newPileList.flatten))
        } else {
          Left(listResult.collect { case Left(ex) => ex }.reduce((a1, a2) =>
            FAtomicException(a1.typeTags ::: a2.typeTags)))
        }
    }
  }

  def genTree[U](pathGen: FAtomicPath => FQueryTranform[U], pile: FPile): TransResult[TransPileWrap] = {
    genTreeTailCall(pathGen, pile, pile) //.right.map { case (newPile, piles) => newPile -> piles }
  }

  def transformTreeList[U, T](pathGen: FAtomicPath => FQueryTranform[U])(columnGen: List[U] => T): FPileSyntax.PileGen[T] = new FPileSyntax.PileGen[T] {
    override def gen(prePiles: List[FPile]) = {
      //防止定义 FPile 时最后一步使用了混合后不能识别最后一层 path
      val piles = prePiles //.flatMap(eachPile => eachPile.genPiles)

      val calculatePiles = piles.map { s =>
        genTree(pathGen, s)
      }.foldLeft(Right(Nil): TransResult[List[TransPileWrap]]) {
        (append, eitherResult) =>
          (append -> eitherResult) match {
            case (Left(s), Left(t)) =>
              Left(FAtomicException(s.typeTags ::: t.typeTags))
            case (Left(s), Right(_)) =>
              Left(FAtomicException(s.typeTags))
            case (Right(_), Left(s)) =>
              Left(FAtomicException(s.typeTags))
            case (Right(s), Right(t)) =>
              Right(t :: s)
          }
      }.right.map(_.reverse)
      calculatePiles.right.map { pileList =>
        val (newPile, summaryPiles) = pileList.map(s => s.root -> s.drops).unzip
        FPileSyntax.PilePip(newPile, { anyList: List[FAtomicValue] =>
          columnGen(ListUtils.splitList(anyList, summaryPiles.map(_.map(_.dataLengthSum).sum): _*)
            .zip(summaryPiles)
            .flatMap {
              case (subList, subPiles) =>
                ListUtils.splitList(subList, subPiles.map(_.dataLengthSum): _*).zip(subPiles).flatMap {
                  case (eachList, eachPiles) =>
                    eachPiles.selfPaths.map(s => pathGen(s)).zip(eachPiles.dataListFromSubList(eachList)).map {
                      case (tranform, data) =>
                        tranform.apply(tranform.gen.right.get, data.asInstanceOf[FAtomicValueImpl[tranform.path.DataType]])
                    }
                }
              //???
            })
          //???
        })
      }

    }

  }

  def transformOf[U, T](pathGen: FAtomicPath => FQueryTranform[U])(columnGen: List[U] => T): List[FAtomicPath] => TransResult[List[FAtomicValue] => T] = {
    (initPaths: List[FAtomicPath]) =>
      {
        initPaths.map(pathGen).zipWithIndex.foldLeft(Right { _: List[FAtomicValue] => Nil }: Either[FAtomicException, List[FAtomicValue] => List[U]]) {
          case (convert, (queryTranform, index)) =>
            (convert -> queryTranform.gen) match {
              case (Left(s), Left(t)) =>
                Left(FAtomicException(s.typeTags ::: t.typeTags))
              case (Left(s), Right(_)) =>
                Left(FAtomicException(s.typeTags))
              case (Right(_), Left(s)) =>
                Left(FAtomicException(s.typeTags))
              case (Right(s), Right(t)) =>
                Right { list: List[FAtomicValue] =>
                  queryTranform.apply(t, list(index).asInstanceOf[FAtomicValueImpl[queryTranform.path.DataType]]) :: s(list)
                }
            }
        }.right.map { s => (t: List[FAtomicValue]) => {
          columnGen(s(t))
        }
        }
      }
  }

  /*def genTreeTailCallWithoutData[U](pathGen: FAtomicPath => FQueryTranformWithOutData[U], oldPile: FPile, newPile: FPile): Either[FAtomicException, (FPile, FPile, List[FPile])] = {
    /*if (newPile.subs.isEmpty) {
      val transforms = newPile.paths.map(pathGen)
      if (transforms.forall(_.gen.isRight)) {
        Right(oldPile, newPile, List(oldPile))
      } else {
        Left(FAtomicException(transforms.map(_.gen).collect { case Left(FAtomicException(s)) => s }.flatten))
      }
    } else {*/
    /*if (newPile.subs.isEmpty) {
      val transforms = newPile.paths.map(pathGen)
      if (transforms.forall(_.gen.isRight)) {
        Right(oldPile, newPile, List(oldPile))
      } else {
        Left(FAtomicException(transforms.map(_.gen).collect { case Left(FAtomicException(s)) => s }.flatten))
      }
    } else {
      val newSubs = oldPile.subs.flatMap(_.genPiles).zip(newPile.subs.flatMap(_.genPiles)).map { case (eachOldPile, eachNewPile) => genTreeTailCallWithoutData(pathGen, eachOldPile, eachNewPile) }
      //val newSubs = oldPile.subs.zip(newPile.subs).map { case (eachOldPile, eachNewPile) => genTreeTailCallWithoutData(pathGen, eachOldPile, eachNewPile) }
      if (newSubs.forall(_.isRight)) {
        val (_, newSubTree, successNodes) = newSubs.map(_.right.get).unzip3
        val newNode = new FPileImpl(newPile.pathPile, newPile.fShape, newPile.dataFromSub, newSubTree) {
          self =>
          //TODO
          override def genPiles = List(self) //throw new Exception("不应该使用")
        } /*()*/
        Right(oldPile, newNode, successNodes.flatten)
      } else {
        genTreeTailCallWithoutData(pathGen, oldPile, new FPileImpl(newPile.pathPile, newPile.fShape, (_: List[Any]) => newPile.fShape.zero, Nil) {
          self =>
          //TODO
          override def genPiles = List(self) //throw new Exception("不应该使用")
        })
      }
    }*/
    ???
  }

  def genTreeWithoutData[U](pathGen: FAtomicPath => FQueryTranformWithOutData[U], pile: FPile): Either[FAtomicException, (FPile, List[FPile])] = {
    genTreeTailCallWithoutData(pathGen, pile, pile).right.map { case (oldPile, newPile, piles) => newPile -> piles }
  }

  def transformTreeListWithoutData[U, T](pathGen: FAtomicPath => FQueryTranformWithOutData[U])(columnGen: List[U] => T): FPileSyntaxWithoutData.PileGen[T] = {
    prePiles: List[FPile] =>
      //防止定义 FPile 时最后一步使用了混合后不能识别最后一层 path
      /*val piles = prePiles.flatMap(eachPile => eachPile.genPiles)
      val calculatePiles = piles.map { s =>
        genTreeWithoutData(pathGen, s)
      }.foldLeft(Right(Nil): Either[FAtomicException, List[(FPile, List[FPile])]]) {
        (append, eitherResult) =>
          (append -> eitherResult) match {
            case (Left(s), Left(t)) =>
              Left(FAtomicException(s.typeTags ::: t.typeTags))
            case (Left(s), Right(_)) =>
              Left(FAtomicException(s.typeTags))
            case (Right(_), Left(s)) =>
              Left(FAtomicException(s.typeTags))
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

  /*def transformOf[U, T](pathGen: FAtomicPath => FQueryTranform[U])(columnGen: List[U] => T): List[FAtomicPath] => Either[FAtomicException, List[FAtomicValue] => T] = {
     (initPaths: List[FAtomicPath]) =>
       ???
     {
       initPaths.map(pathGen).zipWithIndex.foldLeft(Right { _: List[FAtomicValue] => Nil }: Either[FAtomicException, List[FAtomicValue] => List[U]]) {
         case (convert, (queryTranform, index)) =>
           (convert -> queryTranform.gen) match {
             case (Left(s), Left(t)) =>
               Left(FAtomicException(s.typeTags ::: t.typeTags))
             case (Left(s), Right(_)) =>
               Left(FAtomicException(s.typeTags))
             case (Right(_), Left(s)) =>
               Left(FAtomicException(s.typeTags))
             case (Right(s), Right(t)) =>
               Right { list: List[FAtomicValue] =>
                 queryTranform.apply(t, list(index).asInstanceOf[FAtomicValueImpl[queryTranform.path.DataType]]) :: s(list)
               }
           }
       }.right.map { s => (t: List[FAtomicValue]) => {
         columnGen(s(t))
       }
       }
     }
   }*/
}