package net.scalax.fsn.core

import scala.language.higherKinds
import shapeless._

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

  /*implicit def jfkoajiroejhteiroth[S <: HList, T <: HList, U, W, V <: HList, A[_]](
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
  }*/

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

}

trait PilesTransform[A, B, C, D, E[_]] {

  def transform(tran: A => B): FPileImpl[C, D, E]

}

trait PilesPolyHelper {

  implicit class pilesPolyFuction[E, U, C[_]](piles: E)(implicit fPileShape: FPileShape[E, U, C]) {

    def poly[X, Y](parentPile: X)(implicit fsnShape: FPileShape[X, Y, C], zeroPile: FZeroPile[C]): /*(U => Y) => FPileImpl[X, Y, C]*/ PilesTransform[U, Y, X, Y, C] =
      new PilesTransform[U, Y, X, Y, C] {
        override def transform(cv: (U => Y)) = {
          val newShape = new FsnShape[X, Y, C] {
            override def encodeColumn(pile: X): List[FPath] = {
              fsnShape.encodePiles(pile).flatMap { pile =>
                pile.paths
              }
            }
            /*def decodeColumn(columns: List[FPath]): X = {
          val piles1 = fsnShape.encodePiles(parentPile)
          val dataList = ListUtils.splitList(columns, piles1.map(_.dataLengthSum): _*)
          fsnShape.decodePiles(piles1.zipWithIndex.map {
            case (pile, index) =>
              FPile.apply(pile.fShape.decodeColumn(dataList(index)))(pile.fShape)
          })
        }*/
            override def encodeData(pileData: Y): List[C[Any]] = {
              fsnShape.encodePiles(parentPile).zip(fsnShape.encodeData(pileData)).flatMap {
                case (pile, data) =>
                  pile.fShape.encodeData(data.asInstanceOf[pile.DataType])
              }
            }
            override def decodeData(data: List[C[Any]]): Y = {
              val piles1 = fsnShape.encodePiles(parentPile)
              val dataList = ListUtils.splitList(data, piles1.map(_.dataLengthSum): _*)

              fsnShape.decodeData(piles1.zipWithIndex.map {
                case (pile, index) =>
                  pile.fShape.decodeData(dataList(index))
              })
            }

            def zero: Y = {
              val piles1 = fsnShape.encodePiles(parentPile)
              decodeData(List.fill(piles1.map(_.dataLengthSum).sum)(zeroPile.zero))
            }

            val dataLength: Int = {
              fsnShape.encodePiles(parentPile).map { pile =>
                pile.dataLengthSum
              }.sum
            }

          }
          FPileImpl(
            parentPile,
            newShape,
            { list: List[Any] =>
              cv(fPileShape.decodeData(list))
            },
            fPileShape.encodePiles(piles)
          )
        }
      }
  }

}