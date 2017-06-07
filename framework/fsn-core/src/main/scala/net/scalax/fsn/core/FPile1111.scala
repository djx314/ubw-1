package net.scalax.fsn.core

import net.scalax.fsn.core.ListUtils.WeightData
import shapeless._

sealed abstract trait FPileAbs1111 {
  self =>
  type DataType

  def dataLengthSum: Int = {
    self match {
      case pList: FPileList =>
        pList.encodePiles(pList.pileEntity).map(_.dataLengthSum).sum
      case pile: FLeafPile =>
        pile.fShape.dataLength
      case pile: FPile1111 =>
        pile.subs.dataLengthSum
    }
  }

  def dataListFromSubList(atomicDatas: List[WeightData[FAtomicValue]]): List[WeightData[FAtomicValue]] = {
    /*self match {
      case s: FPileList =>
        //如果是 pileList，直接分组再递归调用
        val piles = s.encodePiles(s.pileEntity)
        val datas = ListUtils.splitWithWeight1111(atomicDatas, piles.map(_.dataLengthSum): _*)
        val pileWithData = if (piles.size == datas.size) {
          piles.zip(datas)
        } else {
          throw new Exception("pile 与数据长度不匹配")
        }
        pileWithData.flatMap {
          case (eachPile, eachData) =>
            eachPile.dataListFromSubList(eachData)
        }
      case s: FPile1111 =>
        //如果是 pile，分情况讨论
        if (s.subs.isEmpty) {
          //如果 pile 没有子 pile，数据无需转换，直接返回自己
          atomicDatas
        } else {
          //如果 pile 有子 pile，先将数据按子 pile 分组
          val subPiles = s.subs
          val datas = ListUtils.splitWithWeight1111(atomicDatas, subPiles.map(_.dataLengthSum): _*)
          val pileWithData = if (subPiles.size == datas.size) {
            subPiles.zip(datas)
          } else {
            throw new Exception("pile 与数据长度不匹配")
          }
          val subData = pileWithData.map {
            case (eachSubPile, data) =>
              eachSubPile match {
                case f: FPile1111 =>
                  val resultDatas = f.dataListFromSubList(data)
                  if (resultDatas.size != 1) {
                    throw new Exception("fpile 的 WeightData 长度不是 1")
                  }
                  f.fShape.decodeData(resultDatas.head.data)
                case f: FPileList =>
                  val subListPiles = f.encodePiles(f.pileEntity)
                  val subListDatas = ListUtils.splitWithWeight1111(data, subListPiles.map(_.dataLengthSum): _*)
                  val pileWithData = if (subListPiles.size == subListDatas.size) {
                    subListPiles.zip(subListDatas)
                  } else {
                    throw new Exception("pile 与数据长度不匹配")
                  }
                  val subPileListDatas = pileWithData.map {
                    case (eachSubPile, eachSubData) =>
                      val resultDatas = eachSubPile.dataListFromSubList(eachSubData)
                      if (resultDatas.size != 1) {
                        throw new Exception("fpile 的 WeightData 长度不是 1")
                      }
                      eachSubPile.fShape.decodeData(resultDatas.head.data)
                  }
                  f.decodePileData(subPileListDatas)
              }
          }
          val currentData = s.dataFromSub(subData)
          List(WeightData(s.fShape.encodeData(currentData), s.dataLengthSum))
        }
    }*/
    ???
  }
}

trait FPileList extends FPileAbs1111 {
  type PileType
  override type DataType

  val pileEntity: PileType

  def encodePiles(piles: PileType): List[FCommonPile]
  def decodePileData(datas: List[Any]): DataType
  def encodePileData(data: DataType): List[Any] = ???

}

case class FPileListImpl[PT, DT](
    override val pileEntity: PT,
    encoder: PT => List[FCommonPile],
    dataDecoder: List[Any] => DT,
    dataEncoder: DT => List[Any]
) extends FPileList {
  override type PileType = PT
  override type DataType = DT

  override def encodePiles(piles: PT): List[FCommonPile] = encoder(piles)
  override def decodePileData(datas: List[Any]): DT = dataDecoder(datas)
  override def encodePileData(data: DataType): List[Any] = dataEncoder(data)
}

abstract trait FCommonPile extends FPileAbs1111 {
  type PathType
  override type DataType

  val pathPile: PathType
  val fShape: FsnShape[PathType, DataType]
  //val dataFromSub: List[Any] => DataType
  //val subs: List[FPileAbs1111]
}

trait FPile1111 extends FCommonPile {
  val subs: FPileAbs1111
  def dataFromSub(subDatas: Any): DataType
}

case class FPile1111Impl[PT, DT](
    override val pathPile: PT,
    override val fShape: FsnShape[PT, DT],
    override val subs: FPileAbs1111,
    dataFromSubFunc: Any => DT
) extends FPile1111 {
  override type PathType = PT
  override type DataType = DT

  override def dataFromSub(subDatas: Any): DataType = dataFromSubFunc(subDatas)

}

trait FLeafPile extends FCommonPile

case class FLeafPileImpl[PT, DT](
    override val pathPile: PT,
    override val fShape: FsnShape[PT, DT]
) extends FLeafPile {
  override type PathType = PT
  override type DataType = DT
}

object FPile1111 {

  def genTreeTailCall1111[U](pathGen: FAtomicPath => FQueryTranform[U], oldPile: FPile, newPile: FPile): Either[FAtomicException, (FPile, FPile, List[FPile])] = {
    if (newPile.subs.isEmpty) {
      val transforms = newPile.paths.map(pathGen)
      if (transforms.forall(_.gen.isRight)) {
        Right(oldPile, newPile, List(oldPile))
      } else {
        Left(FAtomicException(transforms.map(_.gen).collect { case Left(FAtomicException(s)) => s }.flatten))
      }
    } else {
      val newSubs = oldPile.subs.flatMap(_.genPiles).zip(newPile.subs.flatMap(_.genPiles)).map { case (eachOldPile, eachNewPile) => genTreeTailCall(pathGen, eachOldPile, eachNewPile) }
      if (newSubs.forall(_.isRight)) {
        val (_, newSubTree, successNodes) = newSubs.map(_.right.get).unzip3
        val newNode = new FPileImpl(newPile.pathPile, newPile.fShape, newPile.dataFromSub, newSubTree) {
          override val genPiles: List[FPile] = List(this)
        }
        Right(oldPile, newNode, successNodes.flatten)
      } else {
        genTreeTailCall(pathGen, oldPile, new FPileImpl(newPile.pathPile, newPile.fShape, (_: List[Any]) => newPile.fShape.zero, Nil) {
          override val genPiles: List[FPile] = List(this)
        })
      }
    }
  }

  def genTreeTailCall[U](pathGen: FAtomicPath => FQueryTranform[U], oldPile: FPile, newPile: FPile): Either[FAtomicException, (FPile, FPile, List[FPile])] = {
    if (newPile.subs.isEmpty) {
      println(newPile)
      val transforms = newPile.paths.map(pathGen)
      if (transforms.forall(_.gen.isRight)) {
        Right(oldPile, newPile, List(oldPile))
      } else {
        Left(FAtomicException(transforms.map(_.gen).collect { case Left(FAtomicException(s)) => s }.flatten))
      }
    } else {
      val newSubs = oldPile.subs.flatMap(_.genPiles).zip(newPile.subs.flatMap(_.genPiles)).map { case (eachOldPile, eachNewPile) => genTreeTailCall(pathGen, eachOldPile, eachNewPile) }
      if (newSubs.forall(_.isRight)) {
        val (_, newSubTree, successNodes) = newSubs.map(_.right.get).unzip3
        val newNode = new FPileImpl(newPile.pathPile, newPile.fShape, newPile.dataFromSub, newSubTree) {
          override val genPiles: List[FPile] = List(this) //newPile.genPiles
        } /*(newPile.genPiles)*/
        Right(oldPile, newNode, successNodes.flatten)
      } else {
        genTreeTailCall(pathGen, oldPile, new FPileImpl(newPile.pathPile, newPile.fShape, (_: List[Any]) => newPile.fShape.zero, Nil) {
          override val genPiles: List[FPile] = List(this) //newPile.genPiles
        } /*()*/ )
      }
    }
  }

  def genTree[U](pathGen: FAtomicPath => FQueryTranform[U], pile: FPile): Either[FAtomicException, (FPile, List[FPile])] = {
    genTreeTailCall(pathGen, pile, pile).right.map { case (oldPile, newPile, piles) => newPile -> piles }
  }

  def transformTree[U, T](pathGen: FAtomicPath => FQueryTranform[U])(columnGen: List[U] => T): FPile => Either[FAtomicException, (FPile, List[FAtomicValue] => T)] = {
    (pile: FPile) =>
      {
        genTree(pathGen, pile).right.map {
          case (rightPile, piles) =>
            rightPile -> { anyList: List[FAtomicValue] =>
              val newList = ListUtils.splitList(anyList, piles.map(_.dataLengthSum): _*).zip(piles).flatMap { case (data, eachPile) => eachPile.dataListFromSubList(data) }
              transformOf(pathGen)(columnGen)(piles.map(_.paths).flatten).right.get.apply(newList)
            }
        }
      }
  }

  def transformTreeList[U, T](pathGen: FAtomicPath => FQueryTranform[U])(columnGen: List[U] => T): FPileSyntax.PileGen[T] = {
    prePiles: List[FPile] =>
      //防止定义 FPile 时最后一步使用了混合后不能识别最后一层 path
      val piles = prePiles.flatMap(eachPile => eachPile.genPiles)

      val calculatePiles = piles.map { s =>
        genTree(pathGen, s)
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
        newPile -> { anyList: List[FAtomicValue] =>
          println(newPile)
          columnGen(ListUtils.splitList(anyList, summaryPiles.map(_.map(_.dataLengthSum).sum): _*)
            .zip(summaryPiles)
            .flatMap {
              case (subList, subPiles) =>
                ListUtils.splitList(subList, subPiles.map(_.dataLengthSum): _*).zip(subPiles).flatMap {
                  case (eachList, eachPiles) =>
                    eachPiles.paths.map(s => pathGen(s)).zip(eachPiles.dataListFromSubList(eachList)).map {
                      case (tranform, data) =>
                        tranform.apply(tranform.gen.right.get, data.asInstanceOf[FAtomicValueImpl[tranform.path.DataType]])
                    }
                }
            })
        }
      }

  }

  def genTreeTailCallWithoutData[U](pathGen: FAtomicPath => FQueryTranformWithOutData[U], oldPile: FPile, newPile: FPile): Either[FAtomicException, (FPile, FPile, List[FPile])] = {
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
  }

  def transformOf[U, T](pathGen: FAtomicPath => FQueryTranform[U])(columnGen: List[U] => T): List[FAtomicPath] => Either[FAtomicException, List[FAtomicValue] => T] = {
    (initPaths: List[FAtomicPath]) =>
      ???
    /*{
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
    }*/
  }
}