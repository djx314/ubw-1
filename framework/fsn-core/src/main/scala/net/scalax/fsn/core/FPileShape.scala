package net.scalax.fsn.core

import scala.language.higherKinds
import shapeless._

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

    def poly[X, Y](parentPile: X)(implicit fsnShape: FsnShape[X, Y, C]): (U => Y) => FPileImpl[X, Y, C] = { cv: (U => Y) =>
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

}