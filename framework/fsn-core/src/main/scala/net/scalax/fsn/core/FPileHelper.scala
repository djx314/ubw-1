package net.scalax.fsn.core

import scala.language.higherKinds
import scala.language.implicitConversions

trait FPileSyntax[C[_], T] {

  val pilesGen: FPileSyntax.PileGen[C, T]

  def flatMap[S, U](mapPiles: FPileSyntax.PileGen[C, S])(cv: (T, List[C[Any]] => S) => U): FPileSyntax.PileGen[C, U] = {
    (piles: List[FPile[C]]) => {
      pilesGen(piles).right.flatMap { case (newPiles, gen) =>
        mapPiles(newPiles).right.map { case (anOtherNewPiles, anOtherGen) =>
          anOtherNewPiles -> { list: List[C[Any]] =>
            cv(gen(list), anOtherGen)
          }
        }
      }
    }
  }

  def result(piles: List[FPile[C]]): Either[FAtomicException, T] = {
    pilesGen(piles).right.map { s =>
      s._2.apply(piles.flatMap(_.deepZero))
    }
  }

}

object FPileSyntax {

  type PileGen[C[_], T] = List[FPile[C]] => Either[FAtomicException, (List[FPile[C]], List[C[Any]] => T)]

  def apply[C[_], T](piles: FPileSyntax.PileGen[C, T]): FPileSyntax[C, T] = {
    new FPileSyntax[C, T] {
      override val pilesGen = piles
    }
  }

}

trait FPilesGenHelper {

  implicit def pileExtensionMethods[C[_], T](pilesGenList: FPileSyntax.PileGen[C, T]): FPileSyntax[C, T] = {
    new FPileSyntax[C, T] {
      override val pilesGen = pilesGenList
    }
  }

}