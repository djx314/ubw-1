package net.scalax.ubw.core

import cats.Monad

object Channel {

  type PilePip[T] = PilePipImpl[DataPileContent => T]
  type PileGen[T] = PileGenImpl[DataPileContent => T]
  val PilePip = PilePipImpl

  case class PilePipImpl[T](piles: Pile, valueFunc: T)

  trait PileGenImpl[T] {
    def gen(pile: Pile): Either[AtomicException, PilePipImpl[T]]
  }

  object PileGenImpl {
    implicit val pileGenMonadImplicit: Monad[PileGenImpl] = new Monad[PileGenImpl] {
      self =>
      override def pure[A](x: A): PileGenImpl[A] = new PileGenImpl[A] {
        def gen(piles: Pile) = Right(PilePipImpl(piles, x))
      }
      override def flatMap[A, B](fa: PileGenImpl[A])(f: A => PileGenImpl[B]): PileGenImpl[B] = {
        new PileGenImpl[B] {
          def gen(pile: Pile) = {
            fa.gen(pile) match {
              case Left(e) =>
                Left(e)
              case Right(PilePipImpl(newPile, newValuesGen)) =>
                val pileB = f(newValuesGen)
                pileB.gen(newPile)
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

  def compose[T, S, U](pilesGen: Channel.PileGen[T])(mapPiles: Channel.PileGen[S])(cv: (T, DataPileContent => S) => U): Channel.PileGen[U] = {
    val monad = implicitly[Monad[Channel.PileGenImpl]]
    monad.flatMap(pilesGen) { tGen =>
      monad.map(mapPiles) { sGen =>
        { values: DataPileContent =>
          cv(tGen(values), sGen)
        }
      }
    }
  }

}