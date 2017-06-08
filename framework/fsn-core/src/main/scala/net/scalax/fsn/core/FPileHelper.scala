package net.scalax.fsn.core

import shapeless._

import scala.language.higherKinds
import scala.language.implicitConversions

trait FPileSyntax[T] {

  val pilesGen: FPileSyntax.PileGen[T]

  def flatMap[S, U](mapPiles: FPileSyntax.PileGen[S])(cv: (T, List[FAtomicValue] => S) => U): FPileSyntax.PileGen[U] = {
    (piles: List[FPile]) =>
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

  def result(piles: List[FPile]): Either[FAtomicException, T] = {
    pilesGen(piles).right.map { s =>
      s._2.apply(piles.flatMap(_.deepZero))
    }
  }

}

object FPileSyntax {

  type PileGen[T] = List[FPile] => Either[FAtomicException, (List[FPile], List[FAtomicValue] => T)]
  type PileGen1111[T] = List[FPileAbs1111] => Either[FAtomicException, (List[FPileAbs1111], List[FAtomicValue] => T)]

  def apply1111[T](piles: FPileSyntax.PileGen[T]): FPileSyntax[T] = {
    new FPileSyntax[T] {
      override val pilesGen = piles
    }
  }

}

trait FPileSyntaxWithoutData[T] {

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

}

trait FPilesGenHelper {

  implicit def pileExtensionMethods[T](pilesGenList: FPileSyntax.PileGen[T]): FPileSyntax[T] = {
    new FPileSyntax[T] {
      override val pilesGen = pilesGenList
    }
  }

  implicit def pileWithoutDataExtensionMethods[T](pilesGenList: FPileSyntaxWithoutData.PileGen[T]): FPileSyntaxWithoutData[T] = {
    new FPileSyntaxWithoutData[T] {
      override val pilesGen = pilesGenList
    }
  }

}

trait FPilesGenHelper1111 {

  trait FLeafPileListPileMerge[LS, LE, D] {
    val basePilePath: LS
    def toPileList(source: LS): FPileListImpl[LS, D]
    def toLeafPile(source: LS): FLeafPileImpl[LE, D]
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
      override val basePilePath = HNil
      override def toPileList(source: HNil): FPileListImpl[HNil, HNil] = emptyPileHlist
      override def toLeafPile(source: HNil): FLeafPileImpl[HNil, HNil] = leafPile
    }
  }

  trait Abc[G, H, I] {
    def transform(cv: G => I): FPile1111Impl[H, I]
  }

  implicit class helper01[T, G](pileAbs: FPile1111Impl[T, G]) {
    def poly[PT, DT](leafPile: FLeafPileImpl[PT, DT]): Abc[G, PT, DT] = new Abc[G, PT, DT] {
      override def transform(cv: G => DT): FPile1111Impl[PT, DT] = new FPile1111Impl[PT, DT](
        leafPile.pathPile,
        leafPile.fShape,
        pileAbs,
        (s: Any) => cv(s.asInstanceOf[G])
      )
    }
  }

  implicit class helper02[G](pileAbs: FLeafPileImpl[_, G]) {
    def poly[PT, DT](leafPile: FLeafPileImpl[PT, DT]): Abc[G, PT, DT] = new Abc[G, PT, DT] {
      override def transform(cv: G => DT): FPile1111Impl[PT, DT] = new FPile1111Impl[PT, DT](
        leafPile.pathPile,
        leafPile.fShape,
        pileAbs,
        (s: Any) => cv(s.asInstanceOf[G])
      )
    }
  }

  implicit class helper03[G](pileAbs: FPileListImpl[_, G]) {
    def poly[PT, DT](leafPile: FLeafPileImpl[PT, DT]): Abc[G, PT, DT] = new Abc[G, PT, DT] {
      override def transform(cv: G => DT): FPile1111Impl[PT, DT] = new FPile1111Impl[PT, DT](
        leafPile.pathPile,
        leafPile.fShape,
        pileAbs,
        (s: Any) => cv(s.asInstanceOf[G])
      )
    }
  }
  /*implicit class helper04[T <: HList, G <: HList](pileAbs: FLeafPileImpl[T, G]) {
    def ::[PT, GT](otherFPile: FLeafPileImpl[PT, GT]): FLeafPileImpl[PT :: T, GT :: G] = {
      new FLeafPileImpl[PT :: T, GT :: G](
        otherFPile.pathPile :: pileAbs.pathPile,

      )
    }
  }*/
}