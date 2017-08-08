package net.scalax.fsn.core

import cats.Monad
import shapeless._

import scala.language.implicitConversions

trait PileSyntax[T] {

  val pilesGen: PileSyntax.PileGen1111[List[AtomicValue] => T]

  def flatMap1111[S, U](mapPiles: PileSyntax.PileGen[S])(cv: (T, List[AtomicValue] => S) => U): PileSyntax.PileGen[U] =
    new PileSyntax.PileGen[U] {
      def gen(piles: List[Pile]): Either[AtomicException, PileSyntax.PilePip1111[List[AtomicValue] => U]] = {
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
    }

  def flatMap[S, U](mapPiles: PileSyntax.PileGen1111[List[AtomicValue] => S])(cv: (T, List[AtomicValue] => S) => U): PileSyntax.PileGen1111[List[AtomicValue] => U] = {
    val monad = implicitly[Monad[PileSyntax.PileGen1111]]
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

  //case class PilePip[T](piles: List[Pile], atomicValues: List[AtomicValue] => T)
  type PilePip[T] = PilePip1111[List[AtomicValue] => T]
  type PileGen[T] = PileGen1111[List[AtomicValue] => T]
  val PilePip = PilePip1111

  /*trait PileGen[T] {
    def gen(piles: List[Pile]): Either[AtomicException, PilePip[T]]
  }*/

  case class PilePip1111[T](piles: List[Pile], atomicValues: T)

  trait PileGen1111[T] {
    def gen(piles: List[Pile]): Either[AtomicException, PilePip1111[T]]
  }

  object PileGen1111 {
    implicit val pileGenMonadImplicit: Monad[PileGen1111] = new Monad[PileGen1111] {
      self =>
      override def pure[A](x: A): PileGen1111[A] = new PileGen1111[A] {
        def gen(piles: List[Pile]) = Right(PilePip1111(piles, x))
      }
      override def flatMap[A, B](fa: PileGen1111[A])(f: A => PileGen1111[B]): PileGen1111[B] = {
        new PileGen1111[B] {
          def gen(piles: List[Pile]) = {
            fa.gen(piles) match {
              case Left(e) =>
                Left(e)
              case Right(PilePip1111(newPiles, newValuesGen)) =>
                val pileB = f(newValuesGen)
                pileB.gen(newPiles)
            }
          }
        }
      }
      override def tailRecM[A, B](a: A)(f: A => PileGen1111[Either[A, B]]): PileGen1111[B] = {
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

  //type PileGen[T] = List[Pile] => Either[AtomicException, (List[Pile], List[AtomicValue] => T)]

  /*def apply[T](piles: PileSyntax.PileGen[T]): PileSyntax[T] = {
    new PileSyntax[T] {
      override val pilesGen = piles
    }
  }*/

}

trait PileSyntaxWithoutData[T] {

  val pilesGen: PileSyntaxWithoutData.PileGen[T]

  def flatMap[S, U](mapPiles: PileSyntaxWithoutData.PileGen[S])(cv: (T, S) => U): PileSyntaxWithoutData.PileGen[U] =
    new PileSyntaxWithoutData.PileGen[U] {
      override def gen(piles: List[Pile]): Either[AtomicException, PileSyntaxWithoutData.PilePip[U]] = {
        pilesGen.gen(piles).right.flatMap {
          case PileSyntaxWithoutData.PilePip(newPiles, gen) =>
            mapPiles.gen(newPiles).right.map {
              case PileSyntaxWithoutData.PilePip(anOtherNewPiles, anOtherGen) =>
                PileSyntaxWithoutData.PilePip(anOtherNewPiles, {
                  cv(gen, anOtherGen)
                })
            }
        }
      }
    }

  def result(piles: List[Pile]): Either[AtomicException, T] = {
    pilesGen.gen(piles).right.map { s =>
      s.result
    }
  }

}

object PileSyntaxWithoutData {

  case class PilePip[T](piles: List[Pile], result: T)

  trait PileGen[T] {
    def gen(piles: List[Pile]): Either[AtomicException, PilePip[T]]
  }

  /*def apply[C[_], T](piles: PileSyntax.PileGen[T]): PileSyntax[T] = {
    new PileSyntax[T] {
      override val pilesGen = piles
    }
  }*/

}

trait PilesGenHelper {

  implicit def pileExtensionMethods[T](pilesGenList: PileSyntax.PileGen[T]): PileSyntax[T] = {
    new PileSyntax[T] {
      override val pilesGen = pilesGenList
    }
  }

  /*implicit def pileWithoutDataExtensionMethods[T](pilesGenList: PileSyntaxWithoutData.PileGen[T]): PileSyntaxWithoutData[T] = {
    new PileSyntaxWithoutData[T] {
      override val pilesGen = pilesGenList
    }
  }*/

}

trait PilesGenHelper1111 {

  trait FLeafPileListPileMerge[LS <: HList, LE <: HList, D <: HList] {
    self =>
    //val basePilePath: LS
    def toPileList: PileListImpl[LS, D]
    def toLeafPile: FLeafPileImpl[LE, D]

    def ::[LS1 <: HList, LE1 <: HList, D1 <: HList](fLeafPile: FLeafPileListPileMerge[LS1, LE1, D1]): FLeafPileListPileMerge[LS1 :: LS, LE1 :: LE, D1 :: D] = {
      new FLeafPileListPileMerge[LS1 :: LS, LE1 :: LE, D1 :: D] {
        //override val basePilePath: FLeafPileImpl[LE1, D1] :: LS = fLeafPile :: self.basePilePath
        override def toPileList: PileListImpl[LS1 :: LS, D1 :: D] = {
          self.::(fLeafPile.toPileList)
        }
        override def toLeafPile: FLeafPileImpl[LE1 :: LE, D1 :: D] = {
          self.::(fLeafPile.toLeafPile).toLeafPile
        }
      }
    }

    def ::[LE1, D1](fLeafPile: FLeafPileImpl[LE1, D1]): FLeafPileListPileMerge[FLeafPileImpl[LE1, D1] :: LS, LE1 :: LE, D1 :: D] = {
      new FLeafPileListPileMerge[FLeafPileImpl[LE1, D1] :: LS, LE1 :: LE, D1 :: D] {
        //override val basePilePath: FLeafPileImpl[LE1, D1] :: LS = fLeafPile :: self.basePilePath
        override def toPileList: PileListImpl[FLeafPileImpl[LE1, D1] :: LS, D1 :: D] = {
          new PileListImpl[FLeafPileImpl[LE1, D1] :: LS, D1 :: D](
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

    def ::[PE, D1](fPileList: PileListImpl[PE, D1]): PileListImpl[PE :: LS, D1 :: D] = {
      val headPileLenght = fPileList.encodePiles(fPileList.pileEntity).size

      new PileListImpl[PE :: LS, D1 :: D](
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

    def ::[PE, D1](fPileList: BranchPileImpl[PE, D1]): PileListImpl[BranchPileImpl[PE, D1] :: LS, D1 :: D] = {

      new PileListImpl[BranchPileImpl[PE, D1] :: LS, D1 :: D](
        fPileList :: self.toPileList.pileEntity,
        encoder = {
          case head :: tail =>
            fPileList :: self.toPileList.encodePiles(tail)
        },
        decoder = { list =>
          list.head.asInstanceOf[BranchPileImpl[PE, D1]] :: self.toPileList.decodePiles(list.tail)
        },
        dataDecoder = { list =>
          list.head.asInstanceOf[D1] :: self.toPileList.decodePileData(list.tail)
        }
      )
    }
  }

  val FPNil: FLeafPileListPileMerge[HNil, HNil, HNil] = {
    val leafPile = new FLeafPileImpl(HNil, FsnShape.hnilFsnShape)
    val emptyPileHlist = new PileListImpl[HNil, HNil](
      HNil,
      { _ => Nil },
      { _ => HNil },
      { _ => HNil }
    )
    new FLeafPileListPileMerge[HNil, HNil, HNil] {
      //override val basePilePath = HNil
      override def toPileList: PileListImpl[HNil, HNil] = emptyPileHlist
      override def toLeafPile: FLeafPileImpl[HNil, HNil] = leafPile
    }
  }

  trait Abc[G, H, I] {
    def transform(cv: G => I): BranchPileImpl[H, I]
  }

  implicit class helper01[T, G](pileAbs: BranchPileImpl[T, G]) {
    def poly[PT, DT](leafPile: FLeafPileImpl[PT, DT]): Abc[G, PT, DT] = new Abc[G, PT, DT] {
      override def transform(cv: G => DT): BranchPileImpl[PT, DT] = new BranchPileImpl[PT, DT](
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
      override def transform(cv: G => DT): BranchPileImpl[PT, DT] = new BranchPileImpl[PT, DT](
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

  implicit class helper03[G](pileAbs: PileListImpl[_, G]) {
    def poly[PT, DT](leafPile: FLeafPileImpl[PT, DT]): Abc[G, PT, DT] = new Abc[G, PT, DT] {
      override def transform(cv: G => DT): BranchPileImpl[PT, DT] = new BranchPileImpl[PT, DT](
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
      override def transform(cv: G => DT): BranchPileImpl[PT, DT] = new BranchPileImpl[PT, DT](
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