package net.scalax.fsn.core

import cats.Monad

trait PileSyntax[T] {

  val pilesGen: PileSyntax.PileGenImpl[List[AtomicValue] => T]
  /*def flatMap1111[S, U](mapPiles: PileSyntax.PileGen[S])(cv: (T, List[AtomicValue] => S) => U): PileSyntax.PileGen[U] =
    new PileSyntax.PileGen[U] {
      def gen(piles: List[Pile]): Either[AtomicException, PileSyntax.PilePipImpl[List[AtomicValue] => U]] = {
        pilesGen.gen(piles).right.flatMap {
          case PileSyntax.PilePip(newPiles, gen) =>
            mapPiles.gen(newPiles).right.map {
              case PileSyntax.PilePip(anOtherNewPiles, anOtherGen) =>
                PileSyntax.PilePip(anOtherNewPiles, { list: List[AtomicValue] =>
                  cv(gen(list), anOtherGen)
                })
            }
        }
      }
    }*/
  def flatMap[S, U](mapPiles: PileSyntax.PileGenImpl[List[AtomicValue] => S])(cv: (T, List[AtomicValue] => S) => U): PileSyntax.PileGenImpl[List[AtomicValue] => U] = {
    val monad = implicitly[Monad[PileSyntax.PileGenImpl]]
    monad.flatMap(pilesGen) { tGen =>
      monad.map(mapPiles) { sGen =>
        { values: List[AtomicValue] =>
          cv(tGen(values), sGen)
        }
      }
    }
  }

  def result(piles: List[Pile]): Either[AtomicException, T] = {
    pilesGen.gen(piles).right.map { s =>
      s.atomicValues.apply(piles.flatMap(_.deepZero))
    }
  }

}

object PileSyntax {

  type PilePip[T] = PilePipImpl[List[AtomicValue] => T]
  type PileGen[T] = PileGenImpl[List[AtomicValue] => T]
  val PilePip = PilePipImpl

  case class PilePipImpl[T](piles: List[Pile], atomicValues: T)

  trait PileGenImpl[T] {
    def gen(piles: List[Pile]): Either[AtomicException, PilePipImpl[T]]
  }

  object PileGenImpl {
    implicit val pileGenMonadImplicit: Monad[PileGenImpl] = new Monad[PileGenImpl] {
      self =>
      override def pure[A](x: A): PileGenImpl[A] = new PileGenImpl[A] {
        def gen(piles: List[Pile]) = Right(PilePipImpl(piles, x))
      }
      override def flatMap[A, B](fa: PileGenImpl[A])(f: A => PileGenImpl[B]): PileGenImpl[B] = {
        new PileGenImpl[B] {
          def gen(piles: List[Pile]) = {
            fa.gen(piles) match {
              case Left(e) =>
                Left(e)
              case Right(PilePipImpl(newPiles, newValuesGen)) =>
                val pileB = f(newValuesGen)
                pileB.gen(newPiles)
            }
          }
        }
      }
      override def tailRecM[A, B](a: A)(f: A => PileGenImpl[Either[A, B]]): PileGenImpl[B] = {
        val eitherPile = f(a)
        self.flatMap(eitherPile) {
          case Left(a) =>
            tailRecM(a)(f)
          case Right(b) =>
            self.pure(b)
        }
      }
    }
  }

}

object PileSyntax1111 {

  type PilePip[T] = PilePipImpl[List[DataPile] => T]
  type PileGen[T] = PileGenImpl[List[DataPile] => T]
  val PilePip = PilePipImpl

  case class PilePipImpl[T](piles: List[Pile], valueFunc: T)

  trait PileGenImpl[T] {
    def gen(piles: List[Pile]): Either[AtomicException, PilePipImpl[T]]
  }

  object PileGenImpl {
    implicit val pileGenMonadImplicit: Monad[PileGenImpl] = new Monad[PileGenImpl] {
      self =>
      override def pure[A](x: A): PileGenImpl[A] = new PileGenImpl[A] {
        def gen(piles: List[Pile]) = Right(PilePipImpl(piles, x))
      }
      override def flatMap[A, B](fa: PileGenImpl[A])(f: A => PileGenImpl[B]): PileGenImpl[B] = {
        new PileGenImpl[B] {
          def gen(piles: List[Pile]) = {
            fa.gen(piles) match {
              case Left(e) =>
                Left(e)
              case Right(PilePipImpl(newPiles, newValuesGen)) =>
                val pileB = f(newValuesGen)
                pileB.gen(newPiles)
            }
          }
        }
      }
      override def tailRecM[A, B](a: A)(f: A => PileGenImpl[Either[A, B]]): PileGenImpl[B] = {
        val eitherPile = f(a)
        self.flatMap(eitherPile) {
          case Left(a) =>
            tailRecM(a)(f)
          case Right(b) =>
            self.pure(b)
        }
      }
    }
  }

}