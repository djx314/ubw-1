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

  def transform[C[_], S, T](pathGen: FPath => Either[FAtomicException, S])(columnGen: List[S] => T): List[FPath] => Either[FAtomicException, T] = {
    (initPaths: List[FPath]) => {
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
  /*def transformOpt(piles: List[FEntity[Option]] => List[Any]): List[FEntity[Option]] => List[FEntity[Option]] = {
    (oldPiles: List[FEntity[Option]]) => {
      oldPiles.zip(piles(oldPiles)).map { case (entity, data) =>
        FEntity.changeData(entity)(data.asInstanceOf[Option[entity.DataType]])
      }
    }
  }*/
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

object FsnShape {

  val hlistFsnShape: FsnShape[HNil, HNil, Option] = new FsnShape[HNil, HNil, Option] {
    override def encodeColumn(pile: HNil): List[FPath] = Nil
    override def decodeColumn(columns: List[FPath]): HNil = HNil

    override def encodeData(pileData: HNil): List[Option[Any]] = Nil
    override def decodeData(data: List[Option[Any]]): HNil = HNil

    override def zero = HNil

    override val dataLength = 0
  }

  implicit def hlistFsnShapeaa[T]: FsnShape[FPathImpl[T], Option[T], Option] = new FsnShape[FPathImpl[T], Option[T], Option] {
    override def encodeColumn(pile: FPathImpl[T]): List[FPath] = pile :: Nil
    override def decodeColumn(columns: List[FPath]): FPathImpl[T] = columns.head.asInstanceOf[FPathImpl[T]]

    override def encodeData(pileData: Option[T]): List[Option[Any]] = pileData :: Nil
    override def decodeData(data: List[Option[Any]]): Option[T] = data.head.asInstanceOf[Option[T]]

    override def zero = None

    override val dataLength = 1
  }

  implicit def jfkoajiroejhteiroth[S <: HList, T <: HList, U, V <: HList](
    implicit
    cv: S <:< (FPathImpl[U] :: T),
    reverseCv: (FPathImpl[U] :: T) <:< S,
    //subShape: FsnShape[FPathImpl[U], Option[U], Option],
    tailShape: FsnShape[T, V, Option]
  ): FsnShape[S, Option[U] :: V, Option] = new FsnShape[S, Option[U] :: V, Option] {
    override def encodeColumn(pile: S): List[FPath] = {
      val headPile :: hlistPile = cv(pile)
      headPile :: tailShape.encodeColumn(hlistPile)
    }
    override def decodeColumn(columns: List[FPath]): S = {
      reverseCv(columns.head.asInstanceOf[FPathImpl[U]] :: tailShape.decodeColumn(columns.tail))
    }

    override def encodeData(pileData: Option[U] :: V): List[Option[Any]] = {
      val headData :: tailData = pileData
      headData :: tailShape.encodeData(tailData)
    }
    override def decodeData(data: List[Option[Any]]): Option[U] :: V = {
      data.head.asInstanceOf[Option[U]] :: tailShape.decodeData(data.tail)
    }

    override def zero = Option.empty[U] :: tailShape.zero

    override val dataLength = 1 + tailShape.dataLength
  }

  implicit val hlistFsnShapeImplicit: FsnShape[HNil, HNil, Option] = hlistFsnShape

}