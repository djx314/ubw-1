package net.scalax.ubw.core

import cats.{ Functor, Monad }

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

  //def afterResult[E](filter: PileFilter[E])

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