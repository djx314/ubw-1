package net.scalax.fsn.core

import cats.Monad
import shapeless._

import scala.language.implicitConversions

trait FPileSyntax[T] {

  val pilesGen: FPileSyntax.PileGen1111[List[FAtomicValue] => T]

  def flatMap1111[S, U](mapPiles: FPileSyntax.PileGen1111[List[FAtomicValue] => S])(cv: (T, List[FAtomicValue] => S) => U): FPileSyntax.PileGen1111[List[FAtomicValue] => U] = new FPileSyntax.PileGen1111[List[FAtomicValue] => U] {
    def gen(piles: List[FPile]): Either[FAtomicException, FPileSyntax.PilePip1111[List[FAtomicValue] => U]] = {
      pilesGen.gen(piles).right.flatMap {
        case FPileSyntax.PilePip(newPiles, gen) =>
          mapPiles.gen(newPiles).right.map {
            case FPileSyntax.PilePip(anOtherNewPiles, anOtherGen) =>
              FPileSyntax.PilePip(anOtherNewPiles, { list: List[FAtomicValue] =>
                cv(gen(list), anOtherGen)
              })
          }
      }
    }
  }

  def flatMap[S, U](mapPiles: FPileSyntax.PileGen1111[List[FAtomicValue] => S])(cv: (T, List[FAtomicValue] => S) => U): FPileSyntax.PileGen1111[List[FAtomicValue] => U] = {
    val monad = implicitly[Monad[FPileSyntax.PileGen1111]]
    monad.flatMap(pilesGen) { tGen =>
      monad.map(mapPiles) { sGen =>
        { values: List[FAtomicValue] =>
          cv(tGen(values), sGen)
        }
      }
    }
  }

  def result(piles: List[FPile]): Either[FAtomicException, T] = {
    pilesGen.gen(piles).right.map { s =>
      s.atomicValues.apply(piles.flatMap(_.deepZero))
    }
  }

}

object FPileSyntax {

  //case class PilePip[T](piles: List[FPile], atomicValues: List[FAtomicValue] => T)
  type PilePip[T] = PilePip1111[List[FAtomicValue] => T]
  type PileGen[T] = PileGen1111[List[FAtomicValue] => T]
  val PilePip = PilePip1111

  /*trait PileGen[T] {
    def gen(piles: List[FPile]): Either[FAtomicException, PilePip[T]]
  }*/

  case class PilePip1111[T](piles: List[FPile], atomicValues: T)

  trait PileGen1111[T] {
    def gen(piles: List[FPile]): Either[FAtomicException, PilePip1111[T]]
  }

  object PileGen1111 {
    implicit val pileGenMonadImplicit: Monad[PileGen1111] = new Monad[PileGen1111] {
      self =>
      override def pure[A](x: A): PileGen1111[A] = new PileGen1111[A] {
        def gen(piles: List[FPile]) = Right(PilePip1111(piles, x))
      }
      override def flatMap[A, B](fa: PileGen1111[A])(f: A => PileGen1111[B]): PileGen1111[B] = {
        new PileGen1111[B] {
          def gen(piles: List[FPile]) = {
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

  //type PileGen[T] = List[FPile] => Either[FAtomicException, (List[FPile], List[FAtomicValue] => T)]

  /*def apply[T](piles: FPileSyntax.PileGen[T]): FPileSyntax[T] = {
    new FPileSyntax[T] {
      override val pilesGen = piles
    }
  }*/

}

trait FPileSyntaxWithoutData[T] {

  val pilesGen: FPileSyntaxWithoutData.PileGen[T]

  def flatMap[S, U](mapPiles: FPileSyntaxWithoutData.PileGen[S])(cv: (T, S) => U): FPileSyntaxWithoutData.PileGen[U] =
    new FPileSyntaxWithoutData.PileGen[U] {
      override def gen(piles: List[FPile]): Either[FAtomicException, FPileSyntaxWithoutData.PilePip[U]] = {
        pilesGen.gen(piles).right.flatMap {
          case FPileSyntaxWithoutData.PilePip(newPiles, gen) =>
            mapPiles.gen(newPiles).right.map {
              case FPileSyntaxWithoutData.PilePip(anOtherNewPiles, anOtherGen) =>
                FPileSyntaxWithoutData.PilePip(anOtherNewPiles, {
                  cv(gen, anOtherGen)
                })
            }
        }
      }
    }

  def result(piles: List[FPile]): Either[FAtomicException, T] = {
    pilesGen.gen(piles).right.map { s =>
      s.result
    }
  }

}

object FPileSyntaxWithoutData {

  case class PilePip[T](piles: List[FPile], result: T)

  trait PileGen[T] {
    def gen(piles: List[FPile]): Either[FAtomicException, PilePip[T]]
  }

  /*def apply[C[_], T](piles: FPileSyntax.PileGen[T]): FPileSyntax[T] = {
    new FPileSyntax[T] {
      override val pilesGen = piles
    }
  }*/

}

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

    def ::[LS1 <: HList, LE1 <: HList, D1 <: HList](fLeafPile: FLeafPileListPileMerge[LS1, LE1, D1]): FLeafPileListPileMerge[LS1 :: LS, LE1 :: LE, D1 :: D] = {
      new FLeafPileListPileMerge[LS1 :: LS, LE1 :: LE, D1 :: D] {
        //override val basePilePath: FLeafPileImpl[LE1, D1] :: LS = fLeafPile :: self.basePilePath
        override def toPileList: FPileListImpl[LS1 :: LS, D1 :: D] = {
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

    def ::[PE, D1](fPileList: FBranchPileImpl[PE, D1]): FPileListImpl[FBranchPileImpl[PE, D1] :: LS, D1 :: D] = {

      new FPileListImpl[FBranchPileImpl[PE, D1] :: LS, D1 :: D](
        fPileList :: self.toPileList.pileEntity,
        encoder = {
          case head :: tail =>
            fPileList :: self.toPileList.encodePiles(tail)
        },
        decoder = { list =>
          list.head.asInstanceOf[FBranchPileImpl[PE, D1]] :: self.toPileList.decodePiles(list.tail)
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
    def transform(cv: G => I): FBranchPileImpl[H, I]
  }

  implicit class helper01[T, G](pileAbs: FBranchPileImpl[T, G]) {
    def poly[PT, DT](leafPile: FLeafPileImpl[PT, DT]): Abc[G, PT, DT] = new Abc[G, PT, DT] {
      override def transform(cv: G => DT): FBranchPileImpl[PT, DT] = new FBranchPileImpl[PT, DT](
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
      override def transform(cv: G => DT): FBranchPileImpl[PT, DT] = new FBranchPileImpl[PT, DT](
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
      override def transform(cv: G => DT): FBranchPileImpl[PT, DT] = new FBranchPileImpl[PT, DT](
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
      override def transform(cv: G => DT): FBranchPileImpl[PT, DT] = new FBranchPileImpl[PT, DT](
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