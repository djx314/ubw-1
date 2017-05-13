package net.scalax.fsn.core

import scala.language.higherKinds
import scala.language.implicitConversions

trait FPileSyntax[C[_], T] {

  val pilesGen: FPileSyntax.PileGen[C, T]

  def flatMap[S, U](mapPiles: FPileSyntax.PileGen[C, S])(cv: (T, List[C[Any]] => S) => U): FPileSyntax.PileGen[C, U] = {
    (piles: List[FPile[C]]) =>
      {
        pilesGen(piles).right.flatMap {
          case (newPiles, gen) =>
            mapPiles(newPiles).right.map {
              case (anOtherNewPiles, anOtherGen) =>
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

trait FPileSyntax1111[T] {

  val pilesGen: FPileSyntax1111.PileGen[T]

  def flatMap[S, U](mapPiles: FPileSyntax1111.PileGen[S])(cv: (T, List[FAtomicValue] => S) => U): FPileSyntax1111.PileGen[U] = {
    (piles: List[FPile1111]) =>
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

  def result(piles: List[FPile1111]): Either[FAtomicException, T] = {
    pilesGen(piles).right.map { s =>
      s._2.apply(piles.flatMap(_.deepZero))
    }
  }

}

object FPileSyntax1111 {

  type PileGen[T] = List[FPile1111] => Either[FAtomicException, (List[FPile1111], List[FAtomicValue] => T)]

  def apply1111[T](piles: FPileSyntax1111.PileGen[T]): FPileSyntax1111[T] = {
    new FPileSyntax1111[T] {
      override val pilesGen = piles
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

trait FPileSyntaxWithoutData1111[T] {

  val pilesGen: FPileSyntaxWithoutData1111.PileGen[T]

  def flatMap[S, U](mapPiles: FPileSyntaxWithoutData1111.PileGen[S])(cv: (T, S) => U): FPileSyntaxWithoutData1111.PileGen[U] = {
    (piles: List[FPile1111]) =>
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

  def result(piles: List[FPile1111]): Either[FAtomicException, T] = {
    pilesGen(piles).right.map { s =>
      s._2
    }
  }

}

object FPileSyntaxWithoutData1111 {

  type PileGen[T] = List[FPile1111] => Either[FAtomicException, (List[FPile1111], T)]

  def apply[C[_], T](piles: FPileSyntax1111.PileGen[T]): FPileSyntax1111[T] = {
    new FPileSyntax1111[T] {
      override val pilesGen = piles
    }
  }

}

trait FPileSyntaxWithoutData[C[_], T] {

  val pilesGen: FPileSyntaxWithoutData.PileGen[C, T]

  def flatMap[S, U](mapPiles: FPileSyntaxWithoutData.PileGen[C, S])(cv: (T, S) => U): FPileSyntaxWithoutData.PileGen[C, U] = {
    (piles: List[FPile[C]]) =>
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

  def result(piles: List[FPile[C]]): Either[FAtomicException, T] = {
    pilesGen(piles).right.map { s =>
      s._2
    }
  }

}

object FPileSyntaxWithoutData {

  type PileGen[C[_], T] = List[FPile[C]] => Either[FAtomicException, (List[FPile[C]], T)]

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

  implicit def pileWithoutDataExtensionMethods[C[_], T](pilesGenList: FPileSyntaxWithoutData.PileGen[C, T]): FPileSyntaxWithoutData[C, T] = {
    new FPileSyntaxWithoutData[C, T] {
      override val pilesGen = pilesGenList
    }
  }

}