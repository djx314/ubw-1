package net.scalax.fsn.core

import shapeless._

import scala.language.higherKinds
import scala.language.implicitConversions

trait FPileAbstract[C[_]] {
  self =>

  type PathType
  type DataType
  type WrapType[T] = C[T]

  val pathPile: PathType

  val fShape: FsnShape[PathType, DataType /*, PathType*/ , WrapType]

  val dataFromSub: List[Any] => DataType
  val subs: List[FPileAbstract[WrapType]]

  //def genPiles(cusType: self.type): List[FPileAbstract[C]]

}

trait FPile[C[_]] extends FPileAbstract[C] {
  self =>

  val dataListFromSubList: List[C[Any]] => List[C[Any]] = { list =>
    if (subs.isEmpty) {
      list
    } else {
      //println(subs.zip(ListUtils.splitList(list, self.subs.map(_.dataLengthSum): _*)) + "2333")
      //println(self.dataFromSub(subs.zip(ListUtils.splitList(list, self.subs.map(_.dataLengthSum): _*))) + "2333")
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
      fShape.encodeData(fShape.zero /*dataFromSub(List("2333"))*/ )
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

class FPileImpl[E, U, C[_]](
    override val pathPile: E,
    override val fShape: FsnShape[E, U /*, E*/ , C],
    override val dataFromSub: List[Any] => U,
    override val subs: List[FPile[C]]
)(val genPiles: FPileImpl[E, U, C] => List[FPile[C]] = { s: FPileImpl[E, U, C] => List(s) }) extends FPile[C] {
  self =>

  override type PathType = E
  override type DataType = U

  //def genPiles(cusType: FPileImpl[E, U, C]): List[FPile[C]] = cusType :: Nil

  trait Abc[G, H, I] {
    def transform(cv: G => I): FPileImpl[H, I, C]
  }

  def poly[A, B](other: FPileImpl[A, B, C]): Abc[U, A, B] = {
    new Abc[U, A, B] {
      def transform(cv: U => B): FPileImpl[A, B, C] = {
        //println(genPiles(self).map(_.getClass))
        new FPileImpl(other.pathPile, other.fShape, { list: List[Any] =>
          //println(self.dataFromSub(list))
          cv(self.dataFromSub(list))
        }, self.genPiles(self))({ source => other.genPiles(source) }) { tranSelf =>
        }
      }
    }
  }

}

object FPile {

  class bbbb[E <: HList, U <: HList, C[_]](bb: FPileImpl[E, U, C]) {
    /*def ::[B](cc: FAtomicPathImpl[B])(implicit zeroPile: FZeroPile[C[B]]): FPileImpl[FAtomicPathImpl[B] :: E, C[B] :: U, C] = {
      val pathPile = cc :: bb.pathPile
      val shape: FsnShape[FAtomicPathImpl[B] :: E, C[B] :: U, C] = new FsnShape[FAtomicPathImpl[B] :: E, C[B] :: U, C] {
        def encodeColumn(pile: FAtomicPathImpl[B] :: E): List[FAtomicPath] = {
          val sub :: tail = pile
          //println(cc.fShape.encodeColumn(sub).size.toString + "aaaa" + bb.fShape.encodeColumn(tail).size.toString)
          cc :: bb.fShape.encodeColumn(tail)
        }
        def encodeData(pileData: C[B] :: U): List[C[Any]] = {
          val sub :: tail = pileData
          cc :: bb.fShape.encodeData(tail)
        }
        def decodeData(data: List[C[Any]]): C[B] :: U = {
          data.take(1) :: bb.fShape.decodeData(data.drop(1))
        }

        def zero = zeroPile.zero :: bb.fShape.zero

        val dataLength = 1 + bb.fShape.dataLength
      }
      val dataFromSub: List[Any] => (C[B] :: U) = { list =>
        cc.dataFromSub(list.take(cc.subs.size)) :: bb.dataFromSub(list.drop(cc.subs.size))
      }

      FPileImpl(pathPile, shape, dataFromSub, cc.subs ::: bb.subs)
    }*/

    def ::[A, B](cc: FPileImpl[A, B, C]): FPileImpl[A :: E, B :: U, C] = {
      val pathPile = cc.pathPile :: bb.pathPile
      val shape: FsnShape[A :: E, B :: U, C] = new FsnShape[A :: E, B :: U, C] {
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
        //println(list)
        //println(cc.genPiles(cc).size)
        //println(bb.genPiles(bb).size)
        //println(list)
        //println(cc.genPiles(cc).size + " " + bb.genPiles(bb).size)
        val piles = cc.genPiles(cc) ::: bb.genPiles(bb)
        //println(piles.zip(ListUtils.splitList(list, piles.map(_ => 1): _*)))
        shape.decodeData(piles.zip(ListUtils.splitList(list, piles.map(_ => 1): _*)).map {
          case (eachPile, data) =>
            //println(data)
            eachPile.fShape.encodeData(data.head.asInstanceOf[eachPile.DataType])
        }.flatten)
        //list.head.asInstanceOf[B] :: list.tail.head.asInstanceOf[U]
        //cc.dataFromSub(list.take(cc.genPiles(cc).size)) :: bb.dataFromSub(list.drop(cc.genPiles(cc).size))
      }

      new FPileImpl(pathPile, shape, dataFromSub, cc.subs ::: bb.subs)({ cusType =>
        cc.genPiles(cc) ::: bb.genPiles(bb)
      }) /*{ self =>
        override def genPiles(cusType: FPileImpl[A :: E, B :: U, C]): List[FPile[C]] = {
          cc.genPiles(cc) ::: bb.genPiles(bb)
        }
      }*/
    }
  }

  implicit def convert[E <: HList, U <: HList, C[_]](bb: FPileImpl[E, U, C]): bbbb[E, U, C] = new bbbb(bb)

  def empty[C[_]]: FPileImpl[HNil, HNil, C] = {
    val shape = FsnShape.hnilFsnShape[C]
    new FPileImpl(HNil, shape, { _: List[Any] => HNil }, List.empty[FPile[C]])({ cusType =>
      cusType :: Nil
    }) /*{ self =>
      override def genPiles(cusType: FPileImpl[HNil, HNil, C]): List[FPile[C]] = cusType :: Nil
    }*/
  }

  def apply[D, C[_]](paths: FAtomicPathImpl[D])(implicit zeroPile: FZeroPile[C[D]]): FPileImpl[FAtomicPathImpl[D], C[D], C] = {
    val shape = FsnShape.fpathFsnShape[D, C]
    new FPileImpl(paths, shape, { s: List[Any] =>
      //println(s)
      //zeroPile.zero
      s.head.asInstanceOf[C[D]]
    }, List.empty[FPile[C]])({ cusType =>
      cusType :: Nil
    }) /*{ self =>
      override def genPiles(cusType: FPileImpl[FAtomicPathImpl[D], C[D], C]): List[FPile[C]] = cusType :: Nil
    }*/
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
        val newNode = new FPileImpl(newPile.pathPile, newPile.fShape, newPile.dataFromSub, newSubTree)(s => ???)
        Right(oldPile, newNode, successNodes.flatten)
      } else {
        genTreeTailCall(pathGen, oldPile, new FPileImpl(newPile.pathPile, newPile.fShape, (_: List[Any]) => newPile.fShape.zero, Nil)(s => ???))
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
        val newNode = new FPileImpl(newPile.pathPile, newPile.fShape, newPile.dataFromSub, newSubTree)(s => ???)
        Right(oldPile, newNode, successNodes.flatten)
      } else {
        genTreeTailCallWithoutData(pathGen, oldPile, new FPileImpl(newPile.pathPile, newPile.fShape, (_: List[Any]) => newPile.fShape.zero, Nil)(s => ???))
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