package net.scalax.ubw.core

import cats.{ Functor, Monad }
import net.scalax.ubw.core.Channel.PilePipImpl

import scala.language.higherKinds

trait InputChannel[T] {
  self =>

  val pilesGen: Channel.PileGen[T]

  def withSyntax[R[_]](syntax1: PileSyntaxFunctor[T, R]): IOChannel[T, R] = {
    new IOChannel[T, R] {
      override val pilesGen = self.pilesGen
      override val PileSyntaxFunctor = syntax1
    }
  }
  /*def flatMap[S, U](mapPiles: Channel.PileGenImpl[List[DataPile] => S])(cv: (T, List[DataPile] => S) => U): Channel.PileGenImpl[List[DataPile] => U] = {
    val monad = implicitly[Monad[Channel.PileGenImpl]]
    monad.flatMap(pilesGen) { tGen =>
      monad.map(mapPiles) { sGen =>
        { values: List[DataPile] =>
          cv(tGen(values), sGen)
        }
      }
    }
  }*/
  def result(piles: List[Pile]): Either[AtomicException, T] = {
    val listPile = new PileListImpl(
      piles,
      piles.map(_.asInstanceOf[CommonPile]),
      { list: List[Any] => list },
      { list: List[Any] => list }
    )
    commonResult(listPile)
  }

  def commonResult(pile: Pile): Either[AtomicException, T] = {
    pilesGen.gen(pile).right.map { s =>
      s.valueFunc(new DataPileContent {
        override val atomicList = pile.leafZero
        override val oldDataPiles = pile.leafZeroDataPiles
        override val newDataPiles = pile.leafZeroDataPiles
        override val previousContent = Option.empty
      })
    }
  }

}

trait IOChannel[T, R[_]] extends InputChannel[T] {
  self =>
  //override val pilesGen: Channel.PileGen[T]
  val PileSyntaxFunctor: PileSyntaxFunctor[T, R]

  def withFunctor(functor: cats.Functor[R]): FoldableChannel[T, R] = {
    val functor1 = functor
    new FoldableChannel[T, R] {
      override val pilesGen = self.pilesGen
      override val PileSyntaxFunctor = self.PileSyntaxFunctor
      override val functor = functor1
    }
  }

  def next[U](other: InputChannel[U]): InputChannel[R[U]] = {
    new InputChannel[R[U]] {
      override val pilesGen: Channel.PileGen[R[U]] = {
        PileSyntaxFunctor.reduce(self.pilesGen, other.pilesGen)
      }
    }
  }
  /*def afterResult[E](filter: PileFilter[E]): IOChannel[(T, R[E]), ({ type L[K] = (R[K], R[E]) })#L] = {
    new IOChannel[(T, R[E]), ({ type L[K] = (R[K], R[E]) })#L] {
      override val pilesGen = new Channel.PileGen[(T, R[E])] {
        override def gen(pile: Pile): Either[AtomicException, Channel.PilePip[(T, R[E])]] = {
          self.pilesGen.gen(pile) match {
            case Right(oldPile) =>
              Right(PilePipImpl[DataPileContent => (T, R[E])](oldPile.piles, { dataPiles =>
                oldPile.valueFunc(dataPiles) -> self.PileSyntaxFunctor.pileMap(oldPile.valueFunc(dataPiles), { content =>
                  filter.transform(content.newDataPiles)
                })
              }))
            case Left(e) => Left(e)
          }
        }
      }
      override val PileSyntaxFunctor = new PileSyntaxFunctor[(T, R[E]), ({ type L[K] = (R[K], R[E]) })#L] {
        override def pileMap[U](a: (T, R[E]), pervious: DataPileContent => U): (R[U], R[E]) = {
          self.PileSyntaxFunctor.pileMap(a._1, pervious) -> a._2
        }
      }
    }
  }*/
  def afterResult[E](filter: PileFilter[E]): InputChannel[(R[E])] = {
    new InputChannel[R[E]] {
      override val pilesGen = new Channel.PileGen[R[E]] {
        override def gen(pile: Pile): Either[AtomicException, Channel.PilePip[R[E]]] = {
          self.pilesGen.gen(pile) match {
            case Right(oldPile) =>
              Right(PilePipImpl[DataPileContent => R[E]](oldPile.piles, { dataPiles =>
                val selfResult = oldPile.valueFunc(dataPiles)
                self.PileSyntaxFunctor.pileMap(selfResult, { content =>
                  filter.transform(content.newDataPiles)
                })
              }))
            case Left(e) => Left(e)
          }
        }
      }
      /*override val PileSyntaxFunctor = new PileSyntaxFunctor[R[(T, E)], ({ type L[K] = R[R[(K, E)]] })#L] {
        override def pileMap[U](a: R[(T, E)], pervious: DataPileContent => U): R[R[(U, E)]] = {
          self.functor.map(a) {
            case (t, e) =>
              self.PileSyntaxFunctor.pileMap(t, { content => pervious(content) -> e })
          }
        }
      }
      override val functor: cats.Functor[({ type L[K] = R[R[(K, E)]] })#L] = new cats.Functor[({ type L[K] = R[R[(K, E)]] })#L] {
        override def map[A, B](fa: R[R[(A, E)]])(f: A => B): R[R[(B, E)]] = {
          self.functor.map(fa) { t => self.functor.map(t)(t => f(t._1) -> t._2) }
        }
      }*/
    }
  }
}

trait FoldableChannel[T, R[_]] extends IOChannel[T, R] {
  self =>

  override val pilesGen: Channel.PileGen[T]

  override val PileSyntaxFunctor: PileSyntaxFunctor[T, R]

  val functor: cats.Functor[R]

  def next2222[U, H[_]](other: IOChannel[U, H]): IOChannel[R[U], ({ type V[W] = R[H[W]] })#V] = {
    new IOChannel[R[U], ({ type V[W] = R[H[W]] })#V] {
      override val pilesGen: Channel.PileGen[R[U]] = {
        self.PileSyntaxFunctor.reduce(self.pilesGen, other.pilesGen)
      }
      override val PileSyntaxFunctor = new PileSyntaxFunctor[R[U], ({ type V[W] = R[H[W]] })#V] {
        def pileMap[M](a: R[U], pervious: DataPileContent => M): R[H[M]] = {
          self.functor.map(a) { u =>
            other.PileSyntaxFunctor.pileMap(u, pervious)
          }
        }
      }
    }
  }

  def next3333[U, H[_]](other: FoldableChannel[U, H]): FoldableChannel[R[U], ({ type V[W] = R[H[W]] })#V] = {
    new FoldableChannel[R[U], ({ type V[W] = R[H[W]] })#V] {
      override val pilesGen: Channel.PileGen[R[U]] = {
        self.PileSyntaxFunctor.reduce(self.pilesGen, other.pilesGen)
      }
      override val PileSyntaxFunctor = new PileSyntaxFunctor[R[U], ({ type V[W] = R[H[W]] })#V] {
        def pileMap[M](a: R[U], pervious: DataPileContent => M): R[H[M]] = {
          self.functor.map(a) { u =>
            other.PileSyntaxFunctor.pileMap(u, pervious)
          }
        }
      }
      override val functor: cats.Functor[({ type V[W] = R[H[W]] })#V] = {
        new Functor[({ type V[W] = R[H[W]] })#V] {
          override def map[A, B](fa: R[H[A]])(f: (A) => B): R[H[B]] = self.functor.map(fa) { s => other.functor.map(s)(f) }
        }
      }

    }
  }

  /*def afterResult[E](filter: PileFilter[E]): InputChannel[(R[(T, E)]), ({ type L[K] = R[R[(K, E)]] })#L] = {
    new FoldableChannel[R[(T, E)], ({ type L[K] = R[R[(K, E)]] })#L] {
      override val pilesGen = new Channel.PileGen[R[(T, E)]] {
        override def gen(pile: Pile): Either[AtomicException, Channel.PilePip[R[(T, E)]]] = {
          self.pilesGen.gen(pile) match {
            case Right(oldPile) =>
              Right(PilePipImpl[DataPileContent => R[(T, E)]](oldPile.piles, { dataPiles =>
                val selfResult = oldPile.valueFunc(dataPiles)
                self.functor.map(self.PileSyntaxFunctor.pileMap(selfResult, { content =>
                  filter.transform(content.newDataPiles)
                })) { s => selfResult -> s }
              }))
            case Left(e) => Left(e)
          }
        }
      }
      override val PileSyntaxFunctor = new PileSyntaxFunctor[R[(T, E)], ({ type L[K] = R[R[(K, E)]] })#L] {
        override def pileMap[U](a: R[(T, E)], pervious: DataPileContent => U): R[R[(U, E)]] = {
          self.functor.map(a) {
            case (t, e) =>
              self.PileSyntaxFunctor.pileMap(t, { content => pervious(content) -> e })
          }
        }
      }
      override val functor: cats.Functor[({ type L[K] = R[R[(K, E)]] })#L] = new cats.Functor[({ type L[K] = R[R[(K, E)]] })#L] {
        override def map[A, B](fa: R[R[(A, E)]])(f: A => B): R[R[(B, E)]] = {
          self.functor.map(fa) { t => self.functor.map(t)(t => f(t._1) -> t._2) }
        }
      }
    }
  }*/

}

trait PilesGenHelper {
}

trait PileSyntaxFunctor[T, R[_]] extends PilesGenHelper {

  def pileMap[U](a: T, pervious: DataPileContent => U): R[U]

  def reduce[S](pervious: Channel.PileGen[T], next: Channel.PileGen[S]): Channel.PileGen[R[S]] = {
    Channel.compose(pervious)(next) { (t, gen) =>
      pileMap(t, gen)
    }
  }
}