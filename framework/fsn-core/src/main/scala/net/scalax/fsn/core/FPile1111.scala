package net.scalax.fsn.core

import shapeless._

import scala.language.higherKinds
import scala.language.implicitConversions

trait FPileAbstract1111 {
  self =>

  type PathType
  type DataType

  val pathPile: PathType

  val fShape: FsnShape1111[PathType, DataType]

  val dataFromSub: List[Any] => DataType
  val subs: List[FPileAbstract1111]

}

trait FPile1111 extends FPileAbstract1111 {
  self =>

  val dataListFromSubList: List[C[Any]] => List[C[Any]] = { list =>
    if (subs.isEmpty) {
      list
    } else {
      fShape.encodeData(self.dataFromSub(subs.zip(ListUtils.splitList(list, subs.map(_.dataLengthSum): _*)).map {
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
      fShape.encodeData(fShape.zero)
    } else {
      subs.flatMap(_.deepZero)
    }
  }

  override val subs: List[FPile1111]

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

class FPileImpl1111[E, U](
    override val pathPile: E,
    override val fShape: FsnShape1111[E, U],
    override val dataFromSub: List[Any] => U,
    override val subs: List[FPile1111]
)(val genPiles: FPileImpl1111[E, U] => List[FPile1111] = { s: FPileImpl1111[E, U] => List(s) }) extends FPile1111 {
  self =>

  override type PathType = E
  override type DataType = U

  trait Abc[G, H, I] {
    def transform(cv: G => I): FPileImpl1111[H, I, C]
  }

  def poly[A, B](other: FPileImpl1111[A, B, C]): Abc[U, A, B] = {
    new Abc[U, A, B] {
      def transform(cv: U => B): FPileImpl1111[A, B, C] = {
        new FPileImpl1111(other.pathPile, other.fShape, { list: List[Any] =>
          cv(self.dataFromSub(list))
        }, self.genPiles(self))({ source => other.genPiles(source) }) { tranSelf =>
        }
      }
    }
  }

}

object FPile1111 {

  class bbbb[E <: HList, U <: HList, C[_]](bb: FPileImpl1111[E, U, C]) {

    def ::[A, B](cc: FPileImpl1111[A, B, C]): FPileImpl1111[A :: E, B :: U, C] = {
      val pathPile = cc.pathPile :: bb.pathPile
      val shape: FsnShape1111[A :: E, B :: U, C] = new FsnShape1111[A :: E, B :: U, C] {
        def encodeColumn(pile: A :: E): List[FAtomicPath] = {
          val sub :: tail = pile
          cc.fShape.encodeColumn(sub) ::: bb.fShape.encodeColumn(tail)
        }
        def encodeData(pileData: B :: U): List[C[Any]] = {
          val sub :: tail = pileData
          cc.fShape.encodeData(sub) ::: bb.fShape.encodeData(tail)
        }
        def decodeData(data: List[C[Any]]): B :: U = {
          cc.fShape.decodeData(data.take(cc.fShape.dataLength)) :: bb.fShape.decodeData(data.drop(cc.fShape.dataLength))
        }

        def zero = cc.fShape.zero :: bb.fShape.zero

        val dataLength = cc.fShape.dataLength + bb.fShape.dataLength
      }
      val dataFromSub: List[Any] => (B :: U) = { list =>
        val piles = cc.genPiles(cc) ::: bb.genPiles(bb)
        shape.decodeData(piles.zip(ListUtils.splitList(list, piles.map(_ => 1): _*)).map {
          case (eachPile, data) =>
            eachPile.fShape.encodeData(data.head.asInstanceOf[eachPile.DataType])
        }.flatten)
      }

      new FPileImpl1111(pathPile, shape, dataFromSub, cc.subs ::: bb.subs)({ cusType =>
        cc.genPiles(cc) ::: bb.genPiles(bb)
      })
    }
  }

  implicit def convert[E <: HList, U <: HList, C[_]](bb: FPileImpl1111[E, U, C]): bbbb[E, U, C] = new bbbb(bb)

  def empty[C[_]]: FPileImpl1111[HNil, HNil, C] = {
    val shape = FsnShape1111.hnilFsnShape1111[C]
    new FPileImpl1111(HNil, shape, { _: List[Any] => ??? }, List.empty[FPile1111[C]])({ cusType =>
      cusType :: Nil
    })
  }

  def apply[D, C[_]](paths: FAtomicPathImpl[D])(implicit zeroPile: FZeroPile[C[D]]): FPileImpl1111[FAtomicPathImpl[D], C[D], C] = {
    val shape = FsnShape1111.fpathFsnShape1111[D, C]
    new FPileImpl1111(paths, shape, { s: List[Any] =>
      ???
    }, List.empty[FPile1111[C]])({ cusType =>
      cusType :: Nil
    })
  }

  def applyOpt[D](paths: FAtomicPathImpl[D]): FPileImpl1111[FAtomicPathImpl[D], Option[D], Option] = {
    apply(paths)
  }

  def genTreeTailCall[U, C[_]](pathGen: FAtomicPath => FQueryTranform[U, C], oldPile: FPile1111[C], newPile: FPile1111[C]): Either[FAtomicException, (FPile1111[C], FPile1111[C], List[FPile1111[C]])] = {
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
        val newNode = new FPileImpl1111(newPile.pathPile, newPile.fShape, newPile.dataFromSub, newSubTree)(s => ???)
        Right(oldPile, newNode, successNodes.flatten)
      } else {
        genTreeTailCall(pathGen, oldPile, new FPileImpl1111(newPile.pathPile, newPile.fShape, (_: List[Any]) => newPile.fShape.zero, Nil)(s => ???))
      }
    }
  }

  def genTree[U, C[_]](pathGen: FAtomicPath => FQueryTranform[U, C], pile: FPile1111[C]): Either[FAtomicException, (FPile1111[C], List[FPile1111[C]])] = {
    genTreeTailCall(pathGen, pile, pile).right.map { case (oldPile, newPile, piles) => newPile -> piles }
  }

  def transformTree[U, C[_], T](pathGen: FAtomicPath => FQueryTranform[U, C])(columnGen: List[U] => T): FPile1111[C] => Either[FAtomicException, (FPile1111[C], List[C[Any]] => T)] = {
    (pile: FPile1111[C]) =>
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

  def transformTreeList[C[_], U, T](pathGen: FAtomicPath => FQueryTranform[U, C])(columnGen: List[U] => T): FPileSyntax.PileGen1111[C, T] = {
    (piles: List[FPile1111[C]]) =>
      {
        val calculatePiles = piles.map { s =>
          genTree(pathGen, s)
        }.foldLeft(Right(Nil): Either[FAtomicException, List[(FPile1111[C], List[FPile1111[C]])]]) {
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

  def genTreeTailCallWithoutData[C[_], U](pathGen: FAtomicPath => FQueryTranformWithOutData[U, C], oldPile: FPile1111[C], newPile: FPile1111[C]): Either[FAtomicException, (FPile1111[C], FPile1111[C], List[FPile1111[C]])] = {
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
        val newNode = new FPileImpl1111(newPile.pathPile, newPile.fShape, newPile.dataFromSub, newSubTree)(s => ???)
        Right(oldPile, newNode, successNodes.flatten)
      } else {
        genTreeTailCallWithoutData(pathGen, oldPile, new FPileImpl1111(newPile.pathPile, newPile.fShape, (_: List[Any]) => newPile.fShape.zero, Nil)(s => ???))
      }
    }
  }

  def genTreeWithoutData[C[_], U](pathGen: FAtomicPath => FQueryTranformWithOutData[U, C], pile: FPile1111[C]): Either[FAtomicException, (FPile1111[C], List[FPile1111[C]])] = {
    genTreeTailCallWithoutData(pathGen, pile, pile).right.map { case (oldPile, newPile, piles) => newPile -> piles }
  }

  def transformTreeListWithoutData[C[_], U, T](pathGen: FAtomicPath => FQueryTranformWithOutData[U, C])(columnGen: List[U] => T): FPileSyntaxWithoutData.PileGen1111[C, T] = {
    (piles: List[FPile1111[C]]) =>
      {
        val calculatePiles = piles.map { s =>
          genTreeWithoutData(pathGen, s)
        }.foldLeft(Right(Nil): Either[FAtomicException, List[(FPile1111[C], List[FPile1111[C]])]]) {
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