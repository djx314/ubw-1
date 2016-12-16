package net.scalax.fsn.core

import scala.language.higherKinds
import scala.language.implicitConversions
import shapeless._

trait FPileAbstract[C[_]] {
  self =>

  type PathType
  type DataType
  type WrapType[T] = C[T]

  val pathPile: PathType

  val fShape: FsnShape[PathType, DataType, WrapType]

  val dataFromSub: List[Any] => DataType
  val subs: List[FPileAbstract[WrapType]]

}

trait FPile[C[_]] extends FPileAbstract[C] {
  self =>

  val dataListFromSubList: List[C[Any]] => List[C[Any]] = { list =>
    if (subs.isEmpty) {
      list
    } else {
      fShape.encodeData(dataFromSub(subs.zip(FPile.splitList(list, subs.map(_.dataLengthSum): _*)).map { case (eachSub, subData) =>
        eachSub.fShape.decodeData(eachSub.dataListFromSubList(subData))
      }))
    }
  }

  lazy val paths: List[FPath] = fShape.encodeColumn(pathPile)

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

  override val subs: List[FPile[WrapType]]

  override def toString: String = {
    s"""Node(length:${dataLengthSum}): {
       |${ fShape.encodeColumn(pathPile).mkString("\n").split("\\n").map(s => "  " + s).mkString("\n") }
       |""".stripMargin +
      s"""
         |  Children: {
         |${ subs.map(_.toString).mkString("\n").split("\\n").map(s => "    " + s).mkString("\n") }
         |  }
         |}""".stripMargin
  }

}

case class FPileImpl[E, U, C[_]](
  override val pathPile: E,
  override val fShape: FsnShape[E, U, C],
  override val dataFromSub: List[Any] => U,
  override val subs: List[FPile[C]]
) extends FPile[C] {
  override type PathType = E
  override type DataType = U
}

object FPile {

  def splitList[X](spList: List[X], length: Int*): List[List[X]] = {
    val (result, leaveOver) = length.foldLeft(List.empty[List[X]] -> spList) { (a, b) =>
      val (head, tail) = a._2.splitAt(b)
      if (head.size != b) throw new Exception("分离的数组长度不够")
      (head :: a._1) -> tail
    }
    if (! leaveOver.isEmpty) throw new Exception("分离的数组还有剩下的元素")
    result.reverse
  }

  def apply[E, U, C[_]](paths: E)(implicit shape: FsnShape[E, U, C]): FPileImpl[E, U, C] = {
    FPileImpl(paths, shape, { _: List[Any] => shape.zero }, List.empty[FPile[C]])
  }

  def applyOpt[E, U](paths: E)(implicit shape: FsnShape[E, U, Option]): FPileImpl[E, U, Option] = {
    apply(paths)
  }

  def genTreeTailCall[C[_], S](pathGen: FPath => FQueryTranform[S, C], oldPile: FPile[C], newPile: FPile[C]): Either[FAtomicException, (FPile[C], FPile[C], List[FPile[C]])] = {
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

  def genTree[C[_], S](pathGen: FPath => FQueryTranform[S, C], pile: FPile[C]): Either[FAtomicException, (FPile[C], List[FPile[C]])] = {
    genTreeTailCall(pathGen, pile, pile).right.map { case (oldPile, newPile, piles) => newPile -> piles }
  }

  def transformTree[C[_], S, T](pathGen: FPath => FQueryTranform[S, C])(columnGen: List[S] => T): FPile[C] => Either[FAtomicException, (FPile[C], List[C[Any]] => T)] = {
    (pile: FPile[C]) => {
      genTree(pathGen, pile).right.map { case (rightPile, piles) =>
        rightPile -> { anyList: List[C[Any]] =>
          val newList = splitList(anyList, piles.map(_.dataLengthSum): _*).zip(piles).flatMap { case (data, eachPile) => eachPile.dataListFromSubList(data) }
          transformOf(pathGen)(columnGen)(piles.map(_.paths).flatten).right.get.apply(newList)
        }
      }
    }
  }

  def transformTreeList[C[_], S, T](pathGen: FPath => FQueryTranform[S, C])(columnGen: List[S] => T): List[FPile[C]] => Either[FAtomicException, (List[FPile[C]], List[C[Any]] => T)] = {
    (piles: List[FPile[C]]) => {
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
              Right(s ::: t :: Nil)
          }
      }
      calculatePiles.right.map { pileList =>
        val (newPile, summaryPiles) = pileList.unzip
        newPile -> { anyList: List[C[Any]] =>
          columnGen(FPile.splitList(anyList, summaryPiles.map(_.map(_.dataLengthSum).sum): _*)
            .zip(summaryPiles)
            .map { case (subList, subPiles) =>
              FPile.splitList(subList, subPiles.map(_.dataLengthSum): _*).zip(subPiles).map { case (eachList, eachPiles) =>
                eachPiles.paths.map(s => pathGen(s)).zip(eachPiles.dataListFromSubList(eachList)).map { case (tranform, data) =>
                  tranform.apply(tranform.gen.right.get, data.asInstanceOf[C[tranform.fPath.DataType]])
                }
              }
            }.flatten.flatten
          )
        }
      }

    }
  }

  def transformOf[S, T, C[_]](pathGen: FPath => FQueryTranform[S, C])(columnGen: List[S] => T): List[FPath] => Either[FAtomicException, List[C[Any]] => T] = {
    (initPaths: List[FPath]) => {
      initPaths.map(pathGen).zipWithIndex.foldLeft(Right { _: List[C[Any]] => Nil }: Either[FAtomicException, List[C[Any]] => List[S]]) { case (convert, (queryTranform, index)) =>
        (convert -> queryTranform.gen) match {
          case (Left(s), Left(t)) =>
            Left(FAtomicException(s.typeTags ::: t.typeTags))
          case (Left(s), Right(_)) =>
            Left(FAtomicException(s.typeTags))
          case (Right(_), Left(s)) =>
            Left(FAtomicException(s.typeTags))
          case (Right(s), Right(t)) =>
            Right { list: List[C[Any]] =>
              queryTranform.apply(t, list(index).asInstanceOf[C[queryTranform.fPath.DataType]]) :: s(list)
            }
        }
      }.right.map { s =>
        (t: List[C[Any]]) => {
          columnGen(s(t))
        }
      }
    }
  }
}

trait FsnShape[Packed_, DataType_, UnPacked_[_]] {

  type Packed = Packed_
  type UnPacked[T] = UnPacked_[T]
  type DataType = DataType_

  def encodeColumn(pile: Packed_): List[FPath]
  def decodeColumn(columns: List[FPath]): Packed_

  def encodeData(pileData: DataType_): List[UnPacked_[Any]]
  def decodeData(data: List[UnPacked_[Any]]): DataType_

  def zero: DataType_

  val dataLength: Int

}

trait ZeroPile[T] {

  def zero: T

}

object ZeroPile {
  implicit def optionPileZero[T]: ZeroPile[Option[T]] = new ZeroPile[Option[T]] {
    override def zero = None
  }
}

object FsnShape {

  implicit def hlistFsnShape[T[_]]: FsnShape[HNil, HNil, T] = new FsnShape[HNil, HNil, T] {
    override def encodeColumn(pile: HNil): List[FPath] = Nil
    override def decodeColumn(columns: List[FPath]): HNil = HNil

    override def encodeData(pileData: HNil): List[T[Any]] = Nil
    override def decodeData(data: List[T[Any]]): HNil = HNil

    override def zero = HNil

    override val dataLength = 0
  }

  implicit def hlistFsnShapeaa[T, U[_]](implicit zeroPile: ZeroPile[U[T]]): FsnShape[FPathImpl[T], U[T], U] = new FsnShape[FPathImpl[T], U[T], U] {
    override def encodeColumn(pile: FPathImpl[T]): List[FPath] = pile :: Nil
    override def decodeColumn(columns: List[FPath]): FPathImpl[T] = columns.head.asInstanceOf[FPathImpl[T]]

    override def encodeData(pileData: U[T]): List[U[Any]] = pileData.asInstanceOf[U[Any]] :: Nil
    override def decodeData(data: List[U[Any]]): U[T] = data.head.asInstanceOf[U[T]]

    override def zero = zeroPile.zero

    override val dataLength = 1
  }

  implicit def jfkoajiroejhteiroth[S <: HList, T <: HList, U, W, V <: HList, A[_]](
    implicit
    cv: S <:< (U :: T),
    reverseCv: (U :: T) <:< S,
    subShape: FsnShape[U, W, A],
    tailShape: FsnShape[T, V, A]
  ): FsnShape[S, W :: V, A] = new FsnShape[S, W :: V, A] {
    override def encodeColumn(pile: S): List[FPath] = {
      val headPile :: hlistPile = cv(pile)
      subShape.encodeColumn(headPile) ::: tailShape.encodeColumn(hlistPile)
    }
    override def decodeColumn(columns: List[FPath]): S = {
      reverseCv(subShape.decodeColumn(columns.take(subShape.dataLength)) :: tailShape.decodeColumn(columns.drop(subShape.dataLength)))
    }

    override def encodeData(pileData: W :: V): List[A[Any]] = {
      val headData :: tailData = pileData
      subShape.encodeData(headData) ::: tailShape.encodeData(tailData)
    }
    override def decodeData(data: List[A[Any]]): W :: V = {
      subShape.decodeData(data.take(subShape.dataLength)) :: tailShape.decodeData(data.drop(subShape.dataLength))
    }

    override def zero = subShape.zero :: tailShape.zero

    override val dataLength = subShape.dataLength + tailShape.dataLength
  }

}

trait FPileShape[E, U, C[_]] {

  def encodePiles(piles: E): List[FPile[C]]

  def decodeData(data: List[Any]): U

  val dataLength: Int

}

object FPileShape {

  implicit def emptyFPileShape[C[_]]: FPileShape[HNil, HNil, C] = {
    new FPileShape[HNil, HNil, C] {
      override def encodePiles(piles: HNil): List[FPile[C]] = Nil
      override def decodeData(data: List[Any]): HNil = HNil
      override val dataLength = 0
    }
  }

  implicit def fPilePolyShape[E, U, C[_]]: FPileShape[FPileImpl[E, U, C], U, C] = {
    new FPileShape[FPileImpl[E, U, C], U, C] {
      override def encodePiles(pile: FPileImpl[E, U, C]): List[FPile[C]] = pile :: Nil
      override def decodeData(data: List[Any]): U = {
        data.head.asInstanceOf[U]
      }
      override val dataLength = 1
    }
  }

  implicit def jfkoajiroejhteiroth[S <: HList, T <: HList, U, W, V <: HList, A[_]](
    implicit
    cv: S <:< (U :: T),
    reverseCv: (U :: T) <:< S,
    subShape: FPileShape[U, W, A],
    tailShape: FPileShape[T, V, A]
  ): FPileShape[S, W :: V, A] = new FPileShape[S, W :: V, A] {
    override def encodePiles(pile: S): List[FPile[A]] = {
      val sub :: tail = cv(pile)
      subShape.encodePiles(sub) ::: tailShape.encodePiles(tail)
    }
    override def decodeData(data: List[Any]): W :: V = {
      subShape.decodeData(data.take(subShape.dataLength)) :: tailShape.decodeData(data.drop(subShape.dataLength))
    }
    override val dataLength = subShape.dataLength + tailShape.dataLength
  }

}

trait PilesPolyHelper {

  implicit class pilesPolyFuction[E, U, C[_]](piles: E)(implicit fPileShape: FPileShape[E, U, C]) {

    def poly[X, Y](parentPile: FPileImpl[X, Y, C])(cv: U => Y): FPileImpl[X, Y, C] = {
      FPileImpl(
        parentPile.pathPile,
        parentPile.fShape,
        { list: List[Any] =>
          cv(fPileShape.decodeData(list))
        },
        fPileShape.encodePiles(piles)
      )
    }

  }

}