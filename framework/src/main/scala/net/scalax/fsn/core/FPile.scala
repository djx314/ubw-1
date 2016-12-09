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

  val prePile: FPile.SubPileType[DataType, WrapType]

}

case class FPileImpl[E, U, C[_]](
  override val pathPile: E,
  override val fShape: FsnShape[E, U, C],
  override val prePile: FPile.SubPileType[U, C]
) extends FPileAbstract[C] {
  override type PathType = E
  override type DataType = U
}

object FPile {

  type SubPileType[DataType, WrapType[_]] = (List[Any] => DataType, List[FPileAbstract[WrapType]])

  def apply[E, U, C[_]](paths: E)(implicit shape: FsnShape[E, U, C]): FPileImpl[E, U, C] = {
    FPileImpl(paths, shape, { _: List[Any] => shape.zero } -> List.empty[FPileAbstract[C]])
  }

  def applyOpt[E, U](paths: E)(implicit shape: FsnShape[E, U, Option]): FPileImpl[E, U, Option] = {
    apply(paths)
  }

  def transform[C[_], S, T](pathGen: FEntity[C] => Either[FAtomicException, S])(columnGen: List[S] => T): List[FEntity[C]] => Either[FAtomicException, T] = {
    (initPaths: List[FEntity[C]]) => {
      initPaths.map(pathGen).foldLeft(Right(Nil): Either[FAtomicException, List[S]]) { (font, end) =>
        (font -> end) match {
          case (Left(s), Left(t)) =>
            Left(FAtomicException(s.typeTags ::: t.typeTags))
          case (Left(s), Right(_)) =>
            Left(FAtomicException(s.typeTags))
          case (Right(_), Left(s)) =>
            Left(FAtomicException(s.typeTags))
          case (Right(s), Right(t)) =>
            Right(s ::: t :: Nil)
        }
      }.right.map(columnGen)
    }
  }

  def transformOpt[S, T](pathGen: FEntity[Option] => Either[FAtomicException, S])(columnGen: List[S] => T): List[FEntity[Option]] => Either[FAtomicException, T] = {
    transform(pathGen)(columnGen)
  }
}

trait FEntity[C[_]] extends FPath {

  val data: C[DataType]

}

object FEntity {

  def changeData[C[_]](path: FPath)(newData: C[path.DataType]): FEntityImpl[path.DataType, C] = FEntityImpl(path.atomics, newData)

}

case class FEntityImpl[D, E[_]](override val atomics: List[FAtomic[D]], override val data: E[D]) extends FEntity[E] {
  override type DataType = D
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
      subShape.decodeData(data.take(subShape.dataLength)) :: tailShape.decodeData(data.take(subShape.dataLength))
    }

    override def zero = subShape.zero :: tailShape.zero

    override val dataLength = subShape.dataLength + tailShape.dataLength
  }

}