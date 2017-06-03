/*package net.scalax.fsn.core.pile

import net.scalax.fsn.core._
import shapeless._

import scala.language.implicitConversions

trait PathShape[Packed_, DataType_] {
  self =>

  type Packed = Packed_
  type DataType = DataType_

  def encodeColumn(pile: Packed_): List[FAtomicPath]

  def encodeData(pileData: DataType_): List[FAtomicValue]
  def decodeData(data: List[FAtomicValue]): DataType_

  def zero: DataType_

  val dataLength: Int

}

trait FPaths {
  self =>

  type PathType
  type DataType

  val atomicPaths: PathType

  val pathShape: PathShape[PathType, DataType]

}

class FPathsImpl[E, U](
    override val atomicPaths: E,
    override val pathShape: PathShape[E, U]
) extends FPaths {
  override type PathType = E
  override type DataType = U
}

trait FPile {
  self =>

  type CurrPathType
  type CurrDataType
  val paths: CurrPathType
  val subPiles: List[FPile]
  def encodePath(fPaths: CurrPathType): List[FPaths]
  def decodeSubData(fPiles: List[Any]): CurrDataType
}

abstract class FPileImpl[CP, CD](
    override val paths: CP,
    override val subPiles: List[FPile]
) extends FPile {
  override type CurrPathType = CP
  override type CurrDataType = CD

  override def encodePath(fPaths: CP): List[FPaths]
  override def decodeSubData(fPiles: List[Any]): CD

}

object FPile {

  def countSumDataLength(pile: FPile): Int = {
    val subs = pile.subPiles
    if (subs.isEmpty) {
      pile.encodePath(pile.paths).map(s => s.pathShape.dataLength).sum
    } else {
      subs.map(s => countSumDataLength(s)).sum
    }
  }

  def confirmTransform[U](pileWithData: EachPileWithData, pathGen: FAtomicPath => FQueryTranform[U]): Either[FAtomicException, List[U]] = {
    val atomicPaths = pileWithData.pile.encodePath(pileWithData.pile.paths).flatMap(s => s.pathShape.encodeColumn(s.atomicPaths))
    atomicPaths.map(s => pathGen(s)).zipWithIndex.foldLeft(Right(Nil): Either[FAtomicException, List[U]]) {
      case (result, (transform, index)) =>
        (result, transform.gen) match {
          case (Left(s), Left(t)) =>
            Left(FAtomicException(s.typeTags ::: t.typeTags))
          case (Left(s), Right(_)) =>
            Left(FAtomicException(s.typeTags))
          case (Right(_), Left(s)) =>
            Left(FAtomicException(s.typeTags))
          case (Right(s), Right(t)) =>
            val u = transform.apply(t, pileWithData.data(index).asInstanceOf[FAtomicValueImpl[transform.path.DataType]])
            Right(u :: s)
        }
    }
  }

  def deepTransform[U](pileWithData: EachPileWithData, pathGen: FAtomicPath => FQueryTranform[U]): Either[FAtomicException, (EachPileWithData, List[U])] = {
    val subs = pileWithData.pile.subPiles
    if (subs.isEmpty) {
      confirmTransform(pileWithData, pathGen) match {
        case Right(result) => Right(pileWithData, result)
        case Left(s) => Left(s)
      }
    } else {
      val pds = subs.zip(
        ListUtils.splitList(pileWithData.data, subs.map(countSumDataLength): _*)
      ).map(EachPileWithData.tupled)
      val execResult = pds.map { pd =>
        deepTransform(pd, pathGen)
      }
      val isSuccess = execResult.forall(_.isRight)
      val newResult = if (isSuccess) {
        val result = execResult.flatMap(t => t.right.get._2)
        val data = execResult.flatMap(t => t.right.get._1.data)
        val piles = execResult.map(t => t.right.get._1.pile)
        val newPile = new FPileImpl[pileWithData.pile.CurrPathType, pileWithData.pile.CurrDataType](
          pileWithData.pile.paths,
          piles
        ) {

          override def decodeSubData(subData: List[Any]): pileWithData.pile.CurrDataType = {
            pileWithData.pile.decodeSubData(subData)
          }

          override def encodePath(fPaths: pileWithData.pile.CurrPathType): List[FPaths] = pileWithData.pile.encodePath(fPaths)

        }
        Right(EachPileWithData(newPile, data) -> result)
      } else {
        Left(execResult.filter(_.isLeft).map(_.left.get).reduce { (s, t) => FAtomicException(s.typeTags ::: t.typeTags) })
      }
      newResult match {
        case Right(s) => Right(s)
        case Left(_) if subs.flatMap(s => s.subPiles).isEmpty =>
          ???
        case Left(e) => Left(e)
      }

    }
  }

  case class PilesWithData(piles: List[FPile], data: List[FAtomicValue])
  case class EachPileWithData(pile: FPile, data: List[FAtomicValue])

  def genTreeTailCall[U](pathGen: FAtomicPath => FQueryTranform[U], pilesWithData: PilesWithData): Either[FAtomicException, PilesWithData] = {
    val eachPileWD = pilesWithData.piles.zip(
      ListUtils.splitList(pilesWithData.data, pilesWithData.piles.map(countSumDataLength): _*)
    ).map(EachPileWithData.tupled)
    eachPileWD.map {
      case EachPileWithData(pile, data) =>

    }
    /*if (subPiles.isEmpty) {
      val currentPaths = newPile.encodePath(newPile.paths)
      val atomicPaths = currentPaths.flatMap(s => s.pathShape.encodeColumn(s.pathPile))
      val transoforms = atomicPaths.map(s => pathGen(s))
      if (transoforms.forall(_.gen.isRight)) {
        Right(oldPile, newPile, List(oldPile))
      } else {
        Left(FAtomicException(transforms.map(_.gen).collect { case Left(FAtomicException(s)) => s }.flatten))
      }
    } else {
      val newSubs = oldPile.subs.zip(newPile.subs).map { case (eachOldPile, eachNewPile) => genTreeTailCall(pathGen, eachOldPile, eachNewPile) }
      if (newSubs.forall(_.isRight)) {
        val (_, newSubTree, successNodes) = newSubs.map(_.right.get).unzip3
        val newNode = new FPileImpl(newPile.pathPile, newPile.fShape, newPile.dataFromSub, newSubTree) /*(newPile.genPiles)*/
        Right(oldPile, newNode, successNodes.flatten)
      } else {
        genTreeTailCall(pathGen, oldPile, new FPileImpl(newPile.pathPile, newPile.fShape, (_: List[Any]) => newPile.fShape.zero, Nil) /*()*/ )
      }
    }*/
    ???
  }
  /*

  def genTreeTailCall[U](pathGen: FAtomicPath => FQueryTranform[U], oldPile: FPile, newPile: FPile): Either[FAtomicException, (FPile, FPile, List[FPile])] = {
    if (newPile.subs.isEmpty) {
      val transforms = newPile.paths.map(pathGen)
      if (transforms.forall(_.gen.isRight)) {
        Right(oldPile, newPile, List(oldPile))
      } else {
        Left(FAtomicException(transforms.map(_.gen).collect { case Left(FAtomicException(s)) => s }.flatten))
      }
    } else {
      val newSubs = oldPile.subs.zip(newPile.subs).map { case (eachOldPile, eachNewPile) => genTreeTailCall(pathGen, eachOldPile, eachNewPile) }
      if (newSubs.forall(_.isRight)) {
        val (_, newSubTree, successNodes) = newSubs.map(_.right.get).unzip3
        val newNode = new FPileImpl(newPile.pathPile, newPile.fShape, newPile.dataFromSub, newSubTree) /*(newPile.genPiles)*/
        Right(oldPile, newNode, successNodes.flatten)
      } else {
        genTreeTailCall(pathGen, oldPile, new FPileImpl(newPile.pathPile, newPile.fShape, (_: List[Any]) => newPile.fShape.zero, Nil) /*()*/ )
      }
    }
  }

  def genTree[U](pathGen: FAtomicPath => FQueryTranform[U], pile: FPile): Either[FAtomicException, (FPile, List[FPile])] = {
    //println(genTreeTailCall(pathGen, pile, pile))
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
      val piles = prePiles //.flatMap(eachPile => eachPile.genPiles(eachPile))

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
    if (newPile.subs.isEmpty) {
      val transforms = newPile.paths.map(pathGen)
      if (transforms.forall(_.gen.isRight)) {
        Right(oldPile, newPile, List(oldPile))
      } else {
        Left(FAtomicException(transforms.map(_.gen).collect { case Left(FAtomicException(s)) => s }.flatten))
      }
    } else {
      val newSubs = oldPile.subs.zip(newPile.subs).map { case (eachOldPile, eachNewPile) => genTreeTailCallWithoutData(pathGen, eachOldPile, eachNewPile) }
      if (newSubs.forall(_.isRight)) {
        val (_, newSubTree, successNodes) = newSubs.map(_.right.get).unzip3
        val newNode = new FPileImpl(newPile.pathPile, newPile.fShape, newPile.dataFromSub, newSubTree) /*()*/
        Right(oldPile, newNode, successNodes.flatten)
      } else {
        genTreeTailCallWithoutData(pathGen, oldPile, new FPileImpl(newPile.pathPile, newPile.fShape, (_: List[Any]) => newPile.fShape.zero, Nil))
      }
    }
  }

  def genTreeWithoutData[U](pathGen: FAtomicPath => FQueryTranformWithOutData[U], pile: FPile): Either[FAtomicException, (FPile, List[FPile])] = {
    genTreeTailCallWithoutData(pathGen, pile, pile).right.map { case (oldPile, newPile, piles) => newPile -> piles }
  }

  def transformTreeListWithoutData[U, T](pathGen: FAtomicPath => FQueryTranformWithOutData[U])(columnGen: List[U] => T): FPileSyntaxWithoutData.PileGen[T] = {
    prePiles: List[FPile] =>
      //防止定义 FPile 时最后一步使用了混合后不能识别最后一层 path
      val piles = prePiles //.flatMap(eachPile => eachPile.genPiles(eachPile))
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
      }
  }

  def transformOf[U, T](pathGen: FAtomicPath => FQueryTranform[U])(columnGen: List[U] => T): List[FAtomicPath] => Either[FAtomicException, List[FAtomicValue] => T] = {
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
  }*/
}*/ 