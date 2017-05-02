package net.scalax.fsn.core

import scala.language.higherKinds
/*import shapeless._

trait FPileShape[E, U, C[_]] {

  def encodePiles(piles: E): List[FPile[C]]
  //def decodePiles(piles: List[FPile[C]]): E

  def encodeData(data: U): List[Any]
  def decodeData(data: List[Any]): U

  val dataLength: Int

}

object FPileShape {

  implicit def emptyFPileShape[C[_]]: FPileShape[HNil, HNil, C] = {
    new FPileShape[HNil, HNil, C] {
      override def encodePiles(piles: HNil): List[FPile[C]] = Nil
      /*override def decodePiles(piles: List[FPile[C]]): HNil = {
        HNil
      }*/
      override def encodeData(data: HNil): List[Any] = {
        Nil
      }
      override def decodeData(data: List[Any]): HNil = HNil
      override val dataLength = 0
    }
  }

  implicit def fPilePolyShape[E, U, C[_]]: FPileShape[FPileImpl[E, U, C], U, C] = {
    new FPileShape[FPileImpl[E, U, C], U, C] {
      override def encodePiles(pile: FPileImpl[E, U, C]): List[FPile[C]] = pile :: Nil
      /*override def decodePiles(piles: List[FPile[C]]): FPileImpl[E, U, C] = {
        piles.head.asInstanceOf[FPileImpl[E, U, C]]
      }*/
      override def encodeData(data: U): List[Any] = {
        data :: Nil
      }
      override def decodeData(data: List[Any]): U = {
        data.head.asInstanceOf[U]
      }
      override val dataLength = 1
    }
  }

  implicit def jfkoajiroejhteiroth[T <: HList, U, W, V <: HList, A[_]](
    implicit
    subShape: FPileShape[U, W, A],
    tailShape: FPileShape[T, V, A]
  ): FPileShape[U :: T, W :: V, A] = new FPileShape[U :: T, W :: V, A] {
    override def encodePiles(pile: U :: T): List[FPile[A]] = {
      val sub :: tail = pile
      subShape.encodePiles(sub) ::: tailShape.encodePiles(tail)
    }
    /*override def decodePiles(piles: List[FPile[A]]): S = {
      reverseCv(subShape.decodePiles(piles.take(subShape.dataLength)) :: tailShape.decodePiles(piles.drop(subShape.dataLength)))
    }*/
    override def encodeData(data: W :: V): List[Any] = {
      val (sub :: tail) = data
      subShape.encodeData(sub) ::: tailShape.encodeData(tail)
    }
    override def decodeData(data: List[Any]): W :: V = {
      subShape.decodeData(data.take(subShape.dataLength)) :: tailShape.decodeData(data.drop(subShape.dataLength))
    }
    override lazy val dataLength = subShape.dataLength + tailShape.dataLength
  }

}*/

trait PilesTransform[A, B, C, D, E[_]] {

  def transform(tran: A => B): FPileImpl[C, D, E]

}

trait PilesPolyHelper {

  /*implicit class pilesPolyFuction[E, U, C[_]](piles: E)(implicit fPileShape: FPileShape[E, U, C]) {

    def poly[X, Y](parentPile: X)(implicit fsnShape: FsnShape[X, Y, C]): PilesTransform[U, Y, X, Y, C] =
      new PilesTransform[U, Y, X, Y, C] {
        override def transform(cv: (U => Y)): FPileImpl[X, Y, C] = {
          FPileImpl(
            parentPile,
            fsnShape,
            { list: List[Any] =>
              cv(fPileShape.decodeData(list))
            },
            fPileShape.encodePiles(piles)
          )
        }
      }
  }*/

  implicit class pilesPolyFuction[E](piles: E) {

    def poly[X, Y, Z, U, T, C[_]](parentPile: X)(
      implicit
      subShape: FsnShape[E, U, T, C],
      parentShape: FsnShape[X, Y, Z, C]
    ): PilesTransform[U, Y, Z, Y, C] =
      new PilesTransform[U, Y, Z, Y, C] {
        override def transform(cv: (U => Y)): FPileImpl[Z, Y, C] = {
          FPileImpl(
            parentShape.toTarget(parentPile),
            parentShape.packageShape,
            { list: List[Any] =>
              cv(subShape.decodeData(subShape.genPiles(piles).zip(list).flatMap {
                case (pile, pileData) =>
                  pile.fShape.encodeData(pileData.asInstanceOf[pile.DataType])
              }))
              //cv(fPileShape.decodeData(list))
            },
            subShape.genPiles(piles)
          )
        }
      }
    /*def polyOpt[X, Y, Z, U, T](parentPile: X)(
      implicit
      subShape: FsnShape[E, U, T, Option],
      parentShape: FsnShape[X, Y, Z, Option]
    ): PilesTransform[U, Y, Z, Y, Option] =
      new PilesTransform[U, Y, Z, Y, Option] {
        override def transform(cv: (U => Y)): FPileImpl[Z, Y, Option] = {
          FPileImpl(
            parentShape.toTarget(parentPile),
            parentShape.packageShape,
            { list: List[Any] =>
              cv(subShape.decodeData(subShape.genPiles(piles).zip(list).flatMap {
                case (pile, pileData) =>
                  pile.fShape.encodeData(pileData.asInstanceOf[pile.DataType])
              }))
              //cv(fPileShape.decodeData(list))
            },
            subShape.genPiles(piles)
          )
        }
      }*/
  }

  /*implicit class pilesPolyFuction[E, U, T, C[_]](piles: E)(
      implicit
      subShape: FsnShape[E, U, T, C]
  ) {

    def poly[X, Y, Z](parentPile: X)(
      implicit
      parentShape: FsnShape[X, Y, Z, C]
    ): PilesTransform[U, Y, Z, Y, C] =
      new PilesTransform[U, Y, Z, Y, C] {
        override def transform(cv: (U => Y)): FPileImpl[Z, Y, C] = {
          FPileImpl(
            parentShape.toTarget(parentPile),
            parentShape.packageShape, { list: List[Any] =>
              cv(subShape.decodeData(subShape.genPiles(piles).zip(list).flatMap {
                case (pile, pileData) =>
                  pile.fShape.encodeData(pileData.asInstanceOf[pile.DataType])
              }))
              //cv(fPileShape.decodeData(list))
            },
            subShape.genPiles(piles)
          )
        }
      }
  }*/

}