package net.scalax.fsn.core

import shapeless._

import scala.language.higherKinds
import scala.language.implicitConversions

trait FPileSyntax[T] {

  val pilesGen: FPileSyntax.PileGen[T]

  def flatMap[S, U](mapPiles: FPileSyntax.PileGen[S])(cv: (T, List[FAtomicValue] => S) => U): FPileSyntax.PileGen[U] = {
    (piles: List[FPileAbs1111]) =>
      {
        pilesGen(piles).right.flatMap {
          case (newPiles, gen) =>
            mapPiles(newPiles).right.map {
              case (anOtherNewPiles, anOtherGen) =>
                anOtherNewPiles -> { list: List[FAtomicValue] =>
                  cv(gen(list), anOtherGen)
                }
            }
        }
      }
  }

  def result(piles: List[FPileAbs1111]): Either[FAtomicException, T] = {
    pilesGen(piles).right.map { s =>
      s._2.apply(piles.flatMap(_.deepZero))
    }
  }

}

object FPileSyntax {

  //type PileGen[T] = List[FPile] => Either[FAtomicException, (List[FPile], List[FAtomicValue] => T)]
  type PileGen[T] = List[FPileAbs1111] => Either[FAtomicException, (List[FPileAbs1111], List[FAtomicValue] => T)]

  def apply[T](piles: FPileSyntax.PileGen[T]): FPileSyntax[T] = {
    new FPileSyntax[T] {
      override val pilesGen = piles
    }
  }

}

/*trait FPileSyntaxWithoutData[T] {

  val pilesGen: FPileSyntaxWithoutData.PileGen[T]

  def flatMap[S, U](mapPiles: FPileSyntaxWithoutData.PileGen[S])(cv: (T, S) => U): FPileSyntaxWithoutData.PileGen[U] = {
    (piles: List[FPile]) =>
      {
        pilesGen(piles).right.flatMap {
          case (newPiles, gen) =>
            mapPiles(newPiles).right.map {
              case (anOtherNewPiles, anOtherGen) =>
                anOtherNewPiles -> {
                  cv(gen, anOtherGen)
                }
            }
        }
      }
  }

  def result(piles: List[FPile]): Either[FAtomicException, T] = {
    pilesGen(piles).right.map { s =>
      s._2
    }
  }

}

object FPileSyntaxWithoutData {

  type PileGen[T] = List[FPile] => Either[FAtomicException, (List[FPile], T)]

  def apply[C[_], T](piles: FPileSyntax.PileGen[T]): FPileSyntax[T] = {
    new FPileSyntax[T] {
      override val pilesGen = piles
    }
  }

}*/

trait FPilesGenHelper {

  implicit def pileExtensionMethods[T](pilesGenList: FPileSyntax.PileGen[T]): FPileSyntax[T] = {
    new FPileSyntax[T] {
      override val pilesGen = pilesGenList
    }
  }

  /*implicit def pileWithoutDataExtensionMethods[T](pilesGenList: FPileSyntaxWithoutData.PileGen[T]): FPileSyntaxWithoutData[T] = {
    new FPileSyntaxWithoutData[T] {
      override val pilesGen = pilesGenList
    }
  }*/

}

trait FPilesGenHelper1111 {

  trait FLeafPileListPileMerge[LS <: HList, LE <: HList, D <: HList] {
    self =>
    //val basePilePath: LS
    def toPileList: FPileListImpl[LS, D]
    def toLeafPile: FLeafPileImpl[LE, D]

    def ::[LE1, D1](fLeafPile: FLeafPileImpl[LE1, D1]): FLeafPileListPileMerge[FLeafPileImpl[LE1, D1] :: LS, LE1 :: LE, D1 :: D] = {
      new FLeafPileListPileMerge[FLeafPileImpl[LE1, D1] :: LS, LE1 :: LE, D1 :: D] {
        //override val basePilePath: FLeafPileImpl[LE1, D1] :: LS = fLeafPile :: self.basePilePath
        override def toPileList: FPileListImpl[FLeafPileImpl[LE1, D1] :: LS, D1 :: D] = {
          new FPileListImpl[FLeafPileImpl[LE1, D1] :: LS, D1 :: D](
            pileEntity = fLeafPile :: self.toPileList.pileEntity,
            encoder = {
            case head :: tail =>
              head :: self.toPileList.encodePiles(tail)
          },
            decoder = { list =>
            list.head.asInstanceOf[FLeafPileImpl[LE1, D1]] :: self.toPileList.decodePiles(list.tail)
          },
            dataDecoder = { list =>
            list.head.asInstanceOf[D1] :: self.toPileList.decodePileData(list.tail)
          }
          )
        }
        override def toLeafPile: FLeafPileImpl[LE1 :: LE, D1 :: D] = {
          val headLeafPile = self.toLeafPile
          new FLeafPileImpl[LE1 :: LE, D1 :: D](
            fLeafPile.pathPile :: headLeafPile.pathPile,
            FsnShape.fpathHListFsnShape(fLeafPile.fShape, headLeafPile.fShape)
          )
        }
      }
    }

    def ::[PE, D1](fPileList: FPileListImpl[PE, D1]): FPileListImpl[PE :: LS, D1 :: D] = {
      val headPileLenght = fPileList.encodePiles(fPileList.pileEntity).size

      new FPileListImpl[PE :: LS, D1 :: D](
        fPileList.pileEntity :: self.toPileList.pileEntity,
        encoder = {
          case head :: tail =>
            fPileList.encodePiles(head) ::: self.toPileList.encodePiles(tail)
        },
        decoder = { list =>
          fPileList.decodePiles(list.take(headPileLenght)) :: self.toPileList.decodePiles(list.drop(headPileLenght))
        },
        dataDecoder = { list =>
          fPileList.decodePileData(list.take(headPileLenght)) :: self.toPileList.decodePileData(list.drop(headPileLenght))
        }
      )
    }

    def ::[PE, D1](fPileList: FPileImpl[PE, D1]): FPileListImpl[FPileImpl[PE, D1] :: LS, D1 :: D] = {

      new FPileListImpl[FPileImpl[PE, D1] :: LS, D1 :: D](
        fPileList :: self.toPileList.pileEntity,
        encoder = {
          case head :: tail =>
            fPileList :: self.toPileList.encodePiles(tail)
        },
        decoder = { list =>
          list.head.asInstanceOf[FPileImpl[PE, D1]] :: self.toPileList.decodePiles(list.tail)
        },
        dataDecoder = { list =>
          list.head.asInstanceOf[D1] :: self.toPileList.decodePileData(list.tail)
        }
      )
    }
  }

  val FPNil: FLeafPileListPileMerge[HNil, HNil, HNil] = {
    val leafPile = new FLeafPileImpl(HNil, FsnShape.hnilFsnShape)
    val emptyPileHlist = new FPileListImpl[HNil, HNil](
      HNil,
      { _ => Nil },
      { _ => HNil },
      { _ => HNil }
    )
    new FLeafPileListPileMerge[HNil, HNil, HNil] {
      //override val basePilePath = HNil
      override def toPileList: FPileListImpl[HNil, HNil] = emptyPileHlist
      override def toLeafPile: FLeafPileImpl[HNil, HNil] = leafPile
    }
  }

  trait Abc[G, H, I] {
    def transform(cv: G => I): FPileImpl[H, I]
  }

  implicit class helper01[T, G](pileAbs: FPileImpl[T, G]) {
    def poly[PT, DT](leafPile: FLeafPileImpl[PT, DT]): Abc[G, PT, DT] = new Abc[G, PT, DT] {
      override def transform(cv: G => DT): FPileImpl[PT, DT] = new FPileImpl[PT, DT](
        leafPile.pathPile,
        leafPile.fShape,
        pileAbs,
        (s: Any) => cv(s.asInstanceOf[G])
      )
    }

    def poly[PT <: HList, DT <: HList](mergePile: FLeafPileListPileMerge[_, PT, DT]): Abc[G, PT, DT] = {
      poly(mergePile.toLeafPile)
    }
  }

  implicit class helper02[G](pileAbs: FLeafPileImpl[_, G]) {
    def poly[PT, DT](leafPile: FLeafPileImpl[PT, DT]): Abc[G, PT, DT] = new Abc[G, PT, DT] {
      override def transform(cv: G => DT): FPileImpl[PT, DT] = new FPileImpl[PT, DT](
        leafPile.pathPile,
        leafPile.fShape,
        pileAbs,
        (s: Any) => cv(s.asInstanceOf[G])
      )
    }

    def poly[PT <: HList, DT <: HList](mergePile: FLeafPileListPileMerge[_ <: HList, PT, DT]): Abc[G, PT, DT] = {
      poly(mergePile.toLeafPile)
    }
  }

  implicit class helper03[G](pileAbs: FPileListImpl[_, G]) {
    def poly[PT, DT](leafPile: FLeafPileImpl[PT, DT]): Abc[G, PT, DT] = new Abc[G, PT, DT] {
      override def transform(cv: G => DT): FPileImpl[PT, DT] = new FPileImpl[PT, DT](
        leafPile.pathPile,
        leafPile.fShape,
        pileAbs,
        (s: Any) => cv(s.asInstanceOf[G])
      )
    }

    def poly[PT <: HList, DT <: HList](mergePile: FLeafPileListPileMerge[_ <: HList, PT, DT]): Abc[G, PT, DT] = {
      poly(mergePile.toLeafPile)
    }
  }

  implicit class helper04[G <: HList](pileAbs: FLeafPileListPileMerge[_ <: HList, _ <: HList, G]) {
    def poly[PT, DT](leafPile: FLeafPileImpl[PT, DT]): Abc[G, PT, DT] = new Abc[G, PT, DT] {
      override def transform(cv: G => DT): FPileImpl[PT, DT] = new FPileImpl[PT, DT](
        leafPile.pathPile,
        leafPile.fShape,
        pileAbs.toPileList,
        (s: Any) => cv(s.asInstanceOf[G])
      )
    }

    def poly[PT <: HList, DT <: HList](mergePile: FLeafPileListPileMerge[_ <: HList, PT, DT]): Abc[G, PT, DT] = {
      poly(mergePile.toLeafPile)
    }
  }
}