package net.scalax.ubw.core

import shapeless._

trait PilesGenHelper1111 {

  trait LeafPileListPileMerge[LS <: HList, LE <: HList, D <: HList] {
    self =>
    def toPileList: PileListImpl[LS, D]
    def toLeafPile: LeafPileImpl[LE, D]

    def ::[LS1 <: HList, LE1 <: HList, D1 <: HList](LeafPile: LeafPileListPileMerge[LS1, LE1, D1]): LeafPileListPileMerge[LS1 :: LS, LE1 :: LE, D1 :: D] = {
      new LeafPileListPileMerge[LS1 :: LS, LE1 :: LE, D1 :: D] {
        override def toPileList: PileListImpl[LS1 :: LS, D1 :: D] = {
          self.::(LeafPile.toPileList)
        }
        override def toLeafPile: LeafPileImpl[LE1 :: LE, D1 :: D] = {
          self.::(LeafPile.toLeafPile).toLeafPile
        }
      }
    }

    def ::[LE1, D1](LeafPile: LeafPileImpl[LE1, D1]): LeafPileListPileMerge[LeafPileImpl[LE1, D1] :: LS, LE1 :: LE, D1 :: D] = {
      new LeafPileListPileMerge[LeafPileImpl[LE1, D1] :: LS, LE1 :: LE, D1 :: D] {
        override def toPileList: PileListImpl[LeafPileImpl[LE1, D1] :: LS, D1 :: D] = {
          new PileListImpl[LeafPileImpl[LE1, D1] :: LS, D1 :: D](
            pileEntity = LeafPile :: self.toPileList.pileEntity,
            encoder = {
            LeafPile :: self.toPileList.encodePiles
            /*case head :: tail =>
              head :: self.toPileList.encodePiles(tail)*/
          },
            /*decoder = { list =>
            list.head.asInstanceOf[LeafPileImpl[LE1, D1]] :: self.toPileList.decodePiles(list.tail)
          },*/
            dataDecoder = { list =>
            list.head.asInstanceOf[D1] :: self.toPileList.decodePileData(list.tail)
          },
            dataEncoder = {
            case d1 :: d =>
              d1 :: self.toPileList.encodePileData(d) //.head.asInstanceOf[D1] :: self.toPileList.decodePileData(list.tail)
          }
          )
        }
        override def toLeafPile: LeafPileImpl[LE1 :: LE, D1 :: D] = {
          val headLeafPile = self.toLeafPile
          new LeafPileImpl[LE1 :: LE, D1 :: D](
            LeafPile.pathPile :: headLeafPile.pathPile,
            PileShape.fpathHListPileShape(LeafPile.fShape, headLeafPile.fShape)
          )
        }
      }
    }

    def ::[PE, D1](fPileList: PileListImpl[PE, D1]): PileListImpl[PE :: LS, D1 :: D] = {
      val headPileLenght = fPileList.encodePiles /*(fPileList.pileEntity)*/ .size

      new PileListImpl[PE :: LS, D1 :: D](
        fPileList.pileEntity :: self.toPileList.pileEntity,
        encoder = {
          fPileList.encodePiles ::: self.toPileList.encodePiles
          /*case head :: tail =>
            fPileList.encodePiles(head) ::: self.toPileList.encodePiles(tail)*/
        },
        /*decoder = { list =>
          fPileList.decodePiles(list.take(headPileLenght)) :: self.toPileList.decodePiles(list.drop(headPileLenght))
        },*/
        dataDecoder = { list =>
          fPileList.decodePileData(list.take(headPileLenght)) :: self.toPileList.decodePileData(list.drop(headPileLenght))
        },
        dataEncoder = {
          case d1 :: d =>
            d1 :: self.toPileList.encodePileData(d) //.head.asInstanceOf[D1] :: self.toPileList.decodePileData(list.tail)
        }
      )
    }

    def ::[PE, D1](fPileList: BranchPileImpl[PE, D1]): PileListImpl[BranchPileImpl[PE, D1] :: LS, D1 :: D] = {

      new PileListImpl[BranchPileImpl[PE, D1] :: LS, D1 :: D](
        fPileList :: self.toPileList.pileEntity,
        encoder = {
          fPileList :: self.toPileList.encodePiles
          /*case head :: tail =>
            fPileList :: self.toPileList.encodePiles(tail)*/
        },
        /*decoder = { list =>
          list.head.asInstanceOf[BranchPileImpl[PE, D1]] :: self.toPileList.decodePiles(list.tail)
        },*/
        dataDecoder = { list =>
          list.head.asInstanceOf[D1] :: self.toPileList.decodePileData(list.tail)
        },
        dataEncoder = {
          case d1 :: d =>
            d1 :: self.toPileList.encodePileData(d)
        }
      )
    }
  }

  val FPNil: LeafPileListPileMerge[HNil, HNil, HNil] = {
    val leafPile = new LeafPileImpl(HNil, PileShape.hnilPileShape)
    val emptyPileHlist = new PileListImpl[HNil, HNil](
      HNil,
      Nil,
      { _ => HNil },
      //{ _ => HNil },
      { _ => Nil }
    )
    new LeafPileListPileMerge[HNil, HNil, HNil] {
      //override val basePilePath = HNil
      override def toPileList: PileListImpl[HNil, HNil] = emptyPileHlist
      override def toLeafPile: LeafPileImpl[HNil, HNil] = leafPile
    }
  }

  trait Abc[G, H, I] {
    def transform(cv: G => I): BranchPileImpl[H, I]
  }

  implicit class helper01[T, G](pileAbs: BranchPileImpl[T, G]) {
    def poly[PT, DT](leafPile: LeafPileImpl[PT, DT]): Abc[G, PT, DT] = new Abc[G, PT, DT] {
      override def transform(cv: G => DT): BranchPileImpl[PT, DT] = new BranchPileImpl[PT, DT](
        leafPile.pathPile,
        leafPile.fShape,
        pileAbs,
        (s: Any) => cv(s.asInstanceOf[G])
      )
    }

    def poly[PT <: HList, DT <: HList](mergePile: LeafPileListPileMerge[_, PT, DT]): Abc[G, PT, DT] = {
      poly(mergePile.toLeafPile)
    }
  }

  implicit class helper02[G](pileAbs: LeafPileImpl[_, G]) {
    def poly[PT, DT](leafPile: LeafPileImpl[PT, DT]): Abc[G, PT, DT] = new Abc[G, PT, DT] {
      override def transform(cv: G => DT): BranchPileImpl[PT, DT] = new BranchPileImpl[PT, DT](
        leafPile.pathPile,
        leafPile.fShape,
        pileAbs,
        (s: Any) => cv(s.asInstanceOf[G])
      )
    }

    def poly[PT <: HList, DT <: HList](mergePile: LeafPileListPileMerge[_ <: HList, PT, DT]): Abc[G, PT, DT] = {
      poly(mergePile.toLeafPile)
    }
  }

  implicit class helper03[G](pileAbs: PileListImpl[_, G]) {
    def poly[PT, DT](leafPile: LeafPileImpl[PT, DT]): Abc[G, PT, DT] = new Abc[G, PT, DT] {
      override def transform(cv: G => DT): BranchPileImpl[PT, DT] = new BranchPileImpl[PT, DT](
        leafPile.pathPile,
        leafPile.fShape,
        pileAbs,
        (s: Any) => cv(s.asInstanceOf[G])
      )
    }

    def poly[PT <: HList, DT <: HList](mergePile: LeafPileListPileMerge[_ <: HList, PT, DT]): Abc[G, PT, DT] = {
      poly(mergePile.toLeafPile)
    }
  }

  implicit class helper04[G <: HList](pileAbs: LeafPileListPileMerge[_ <: HList, _ <: HList, G]) {
    def poly[PT, DT](leafPile: LeafPileImpl[PT, DT]): Abc[G, PT, DT] = new Abc[G, PT, DT] {
      override def transform(cv: G => DT): BranchPileImpl[PT, DT] = new BranchPileImpl[PT, DT](
        leafPile.pathPile,
        leafPile.fShape,
        pileAbs.toPileList,
        (s: Any) => cv(s.asInstanceOf[G])
      )
    }

    def poly[PT <: HList, DT <: HList](mergePile: LeafPileListPileMerge[_ <: HList, PT, DT]): Abc[G, PT, DT] = {
      poly(mergePile.toLeafPile)
    }
  }
}