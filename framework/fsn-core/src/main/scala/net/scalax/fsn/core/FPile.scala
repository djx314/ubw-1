package net.scalax.fsn.core

import scala.language.higherKinds

trait FPileAbstract[C[_]] {
  self =>

  type PathType
  type DataType
  type WrapType[T] = C[T]

  val pathPile: PathType

  val fShape: FsnShape[PathType, DataType, PathType, WrapType]

  val dataFromSub: List[Any] => DataType
  val subs: List[FPileAbstract[WrapType]]

}

trait FPile[C[_]] extends FPileAbstract[C] {
  self =>

  val dataListFromSubList: List[C[Any]] => List[C[Any]] = { list =>
    if (subs.isEmpty) {
      list
    } else {
      fShape.encodeData(dataFromSub(subs.zip(ListUtils.splitList(list, subs.map(_.dataLengthSum): _*)).map {
        case (eachSub, subData) =>
          eachSub.fShape.decodeData(eachSub.dataListFromSubList(subData))
      }))
    }
  }

  lazy val paths: List[FAtomicPath] = fShape.encodeColumn(pathPile)

  val dataLengthSum: Int = {
    if (subs.isEmpty) {
      fShape.dataLength
    } else {
      subs.map(_.dataLengthSum).sum
    }
  }

  def deepZero: List[C[Any]] = {
    if (subs.isEmpty) {
      fShape.encodeData(dataFromSub(Nil))
    } else {
      subs.flatMap(_.deepZero)
    }
  }

  override val subs: List[FPile[WrapType]]

  override def toString: String = {
    s"""Node(length:${dataLengthSum}): {
       |${fShape.encodeColumn(pathPile).mkString("\n").split("\\n").map(s => "  " + s).mkString("\n")}
       |""".stripMargin +
      s"""
       |  Children: {
       |${subs.map(_.toString).mkString("\n").split("\\n").map(s => "    " + s).mkString("\n")}
       |  }
       |}""".stripMargin
  }

}

case class FPileImpl[E, U, C[_]](
    override val pathPile: E,
    override val fShape: FsnShape[E, U, E, C],
    override val dataFromSub: List[Any] => U,
    override val subs: List[FPile[C]]
) extends FPile[C] {
  override type PathType = E
  override type DataType = U
}

object FPile {

  def apply[D, C[_]](paths: FAtomicPathImpl[D])(implicit zeroPile: FZeroPile[C[D]]): FPileImpl[FAtomicPathImpl[D], C[D], C] = {
    val shape = implicitly[FsnShape[FAtomicPathImpl[D], C[D], FAtomicPathImpl[D], C]]
    FPileImpl(shape.toTarget(paths), shape.packageShape, { _: List[Any] => shape.zero }, List.empty[FPile[C]])
  }

  def applyOpt[D](paths: FAtomicPathImpl[D]): FPileImpl[FAtomicPathImpl[D], Option[D], Option] = {
    apply(paths)
  }

  def genTreeTailCall[U, C[_]](pathGen: FAtomicPath => FQueryTranform[U, C], oldPile: FPile[C], newPile: FPile[C]): Either[FAtomicException, (FPile[C], FPile[C], List[FPile[C]])] = {
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
        val newNode = FPileImpl(newPile.pathPile, newPile.fShape, newPile.dataFromSub, newSubTree)
        Right(oldPile, newNode, successNodes.flatten)
      } else {
        genTreeTailCall(pathGen, oldPile, FPileImpl(newPile.pathPile, newPile.fShape, (_: List[Any]) => newPile.fShape.zero, Nil))
      }
    }
  }

  def genTree[U, C[_]](pathGen: FAtomicPath => FQueryTranform[U, C], pile: FPile[C]): Either[FAtomicException, (FPile[C], List[FPile[C]])] = {
    genTreeTailCall(pathGen, pile, pile).right.map { case (oldPile, newPile, piles) => newPile -> piles }
  }

  def transformTree[U, C[_], T](pathGen: FAtomicPath => FQueryTranform[U, C])(columnGen: List[U] => T): FPile[C] => Either[FAtomicException, (FPile[C], List[C[Any]] => T)] = {
    (pile: FPile[C]) =>
      {
        genTree(pathGen, pile).right.map {
          case (rightPile, piles) =>
            rightPile -> { anyList: List[C[Any]] =>
              val newList = ListUtils.splitList(anyList, piles.map(_.dataLengthSum): _*).zip(piles).flatMap { case (data, eachPile) => eachPile.dataListFromSubList(data) }
              transformOf(pathGen)(columnGen)(piles.map(_.paths).flatten).right.get.apply(newList)
            }
        }
      }
  }

  def transformTreeList[C[_], U, T](pathGen: FAtomicPath => FQueryTranform[U, C])(columnGen: List[U] => T): FPileSyntax.PileGen[C, T] = {
    (piles: List[FPile[C]]) =>
      {
        val calculatePiles = piles.map { s =>
          genTree(pathGen, s)
        }.foldLeft(Right(Nil): Either[FAtomicException, List[(FPile[C], List[FPile[C]])]]) {
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
          newPile -> { anyList: List[C[Any]] =>
            columnGen(ListUtils.splitList(anyList, summaryPiles.map(_.map(_.dataLengthSum).sum): _*)
              .zip(summaryPiles)
              .map {
                case (subList, subPiles) =>
                  ListUtils.splitList(subList, subPiles.map(_.dataLengthSum): _*).zip(subPiles).map {
                    case (eachList, eachPiles) =>
                      eachPiles.paths.map(s => pathGen(s)).zip(eachPiles.dataListFromSubList(eachList)).map {
                        case (tranform, data) =>
                          tranform.apply(tranform.gen.right.get, data.asInstanceOf[C[tranform.path.DataType]])
                      }
                  }
              }.flatten.flatten)
          }
        }

      }
  }

  def genTreeTailCallWithoutData[C[_], U](pathGen: FAtomicPath => FQueryTranformWithOutData[U, C], oldPile: FPile[C], newPile: FPile[C]): Either[FAtomicException, (FPile[C], FPile[C], List[FPile[C]])] = {
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
        val newNode = FPileImpl(newPile.pathPile, newPile.fShape, newPile.dataFromSub, newSubTree)
        Right(oldPile, newNode, successNodes.flatten)
      } else {
        genTreeTailCallWithoutData(pathGen, oldPile, FPileImpl(newPile.pathPile, newPile.fShape, (_: List[Any]) => newPile.fShape.zero, Nil))
      }
    }
  }

  def genTreeWithoutData[C[_], U](pathGen: FAtomicPath => FQueryTranformWithOutData[U, C], pile: FPile[C]): Either[FAtomicException, (FPile[C], List[FPile[C]])] = {
    genTreeTailCallWithoutData(pathGen, pile, pile).right.map { case (oldPile, newPile, piles) => newPile -> piles }
  }

  def transformTreeListWithoutData[C[_], U, T](pathGen: FAtomicPath => FQueryTranformWithOutData[U, C])(columnGen: List[U] => T): FPileSyntaxWithoutData.PileGen[C, T] = {
    (piles: List[FPile[C]]) =>
      {
        val calculatePiles = piles.map { s =>
          genTreeWithoutData(pathGen, s)
        }.foldLeft(Right(Nil): Either[FAtomicException, List[(FPile[C], List[FPile[C]])]]) {
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
  }

  def transformOf[U, T, C[_]](pathGen: FAtomicPath => FQueryTranform[U, C])(columnGen: List[U] => T): List[FAtomicPath] => Either[FAtomicException, List[C[Any]] => T] = {
    (initPaths: List[FAtomicPath]) =>
      {
        initPaths.map(pathGen).zipWithIndex.foldLeft(Right { _: List[C[Any]] => Nil }: Either[FAtomicException, List[C[Any]] => List[U]]) {
          case (convert, (queryTranform, index)) =>
            (convert -> queryTranform.gen) match {
              case (Left(s), Left(t)) =>
                Left(FAtomicException(s.typeTags ::: t.typeTags))
              case (Left(s), Right(_)) =>
                Left(FAtomicException(s.typeTags))
              case (Right(_), Left(s)) =>
                Left(FAtomicException(s.typeTags))
              case (Right(s), Right(t)) =>
                Right { list: List[C[Any]] =>
                  queryTranform.apply(t, list(index).asInstanceOf[C[queryTranform.path.DataType]]) :: s(list)
                }
            }
        }.right.map { s => (t: List[C[Any]]) => {
          columnGen(s(t))
        }
        }
      }
  }
}