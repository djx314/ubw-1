package net.scalax.fsn.core

import cats.{ Functor, Monad }
import net.scalax.fsn.core.DataPile.{ TransPileWrap, fromPile, genTree }

import scala.language.higherKinds

trait SingleInputChannel[T] extends InputChannel[T] {
  self =>

  override val pilesGen: Channel.PileGen[T] = new Channel.PileGen[T] {
    override def gen(prePiles: Pile): Either[AtomicException, Channel.PilePip[T]] = {
      val piles = prePiles

      val calculatePiles = genTree(pathGen, piles)

      calculatePiles.right.map {
        case TransPileWrap(newPiles, summaryPiles) =>
          Channel.PilePipImpl(piles = newPiles, valueFunc = { oldContent: DataPileContent =>
            //数值在管道流动形成新的管道，但未加上本阶段数值的变化
            val (oldDataPile, _) = summaryPiles.foldLeft(List.empty[DataPile], oldContent.newDataPiles.map(_.data.asInstanceOf[Any])) {
              case ((dataPileList, data), pile) =>
                val (currentDataPile, remainData) = fromPile(pile, data)
                (dataPileList ::: currentDataPile :: Nil) -> remainData
            }

            //求出当前 InputChannel 的值
            val result = oldDataPile.map { pile =>
              val pathWithValue = pile.selfPaths.zip(pile.toAtomicValues(pile.data))
              pathWithValue.map {
                case (path, value) =>
                  val transform = pathGen(path)
                  transform.apply(transform.gen.right.get, value.asInstanceOf[AtomicValueImpl[transform.path.DataType]])
              }
            }.flatten

            //本阶段 List[AtomicValue] => List[DataPile] 的转化器
            val atomicValueGen1 = { newAtomicValues: List[AtomicValue] =>
              oldDataPile.foldLeft(List.empty[DataPile] -> newAtomicValues) {
                case ((newDataPiles, currentValues), oldDataPile) =>
                  val (newDataPile, remainData) = oldDataPile.renderFromList(currentValues)
                  (newDataPiles ::: newDataPile :: Nil) -> remainData
              }._1
            }

            val oldDataPile1 = oldDataPile

            val contentGen = new DataPileContentGen {
              override def atomicValueGen(atomicList: List[AtomicValue]) = atomicValueGen1(atomicList)
              override val oldDataPiles = oldDataPile1
              override val previousContent = Option(oldContent)
            }

            columnGen(result, contentGen)
          })
      }

    }
  }

  type QueryType
  def pathGen(path: AtomicPath): QueryTranform[QueryType]
  def columnGen(tempList: List[QueryType], contentGen: DataPileContentGen): T

  override def withSyntax[R[_]](syntax1: PileSyntaxFunctor[T, R]): SingleIOChannel[T, R] = {
    new SingleIOChannel[T, R] {
      override type QueryType = self.QueryType
      override def pathGen(path: AtomicPath): QueryTranform[QueryType] = self.pathGen(path)
      override def columnGen(tempList: List[QueryType], contentGen: DataPileContentGen): T = self.columnGen(tempList, contentGen)
      override val PileSyntaxFunctor = syntax1
    }
  }

  def withFilter[K](gen: AtomicPath => QueryTranform[K]): SingleInputChannel[(List[K], () => T)] = {
    new SingleInputChannel[(List[K], () => T)] {
      override type QueryType = self.QueryType
      override def pathGen(path: AtomicPath): QueryTranform[QueryType] = self.pathGen(path)
      override def columnGen(tempList: List[QueryType], contentGen: DataPileContentGen): (List[K], () => T) = {
        {
          contentGen.oldDataPiles.flatMap { oldDataPile =>
            oldDataPile.pathWithValues.flatMap { pAndV =>
              val wrap = gen(pAndV.path)
              wrap.gen match {
                case Left(_) => Option.empty[K]
                case Right(tran) =>
                  Option(wrap.apply(tran, pAndV.value.asInstanceOf[AtomicValueImpl[wrap.path.DataType]]))
              }
            }
          }
        } -> { () =>
          self.columnGen(tempList, contentGen)
        }
      }
    }
  }

}

trait SingleIOChannel[T, R[_]] extends SingleInputChannel[T] with IOChannel[T, R] {
  self =>

  val PileSyntaxFunctor: PileSyntaxFunctor[T, R]

  override def withFunctor(functor: cats.Functor[R]): SingleFoldableChannel[T, R] = {
    val functor1 = functor
    new SingleFoldableChannel[T, R] {
      override type QueryType = self.QueryType
      override def pathGen(path: AtomicPath): QueryTranform[QueryType] = self.pathGen(path)
      override def columnGen(tempList: List[QueryType], contentGen: DataPileContentGen): T = self.columnGen(tempList, contentGen)
      override val PileSyntaxFunctor = self.PileSyntaxFunctor
      override val functor = functor1
    }
  }

  override def withFilter[K](gen: AtomicPath => QueryTranform[K]): SingleIOChannel[(List[K], () => T), ({ type L[M] = (List[K], R[M]) })#L] = {
    new SingleIOChannel[(List[K], () => T), ({ type L[M] = (List[K], R[M]) })#L] {
      override val PileSyntaxFunctor = new PileSyntaxFunctor[(List[K], () => T), ({ type L[M] = (List[K], R[M]) })#L] {
        override def pileMap[H](a: (List[K], () => T), pervious: DataPileContent => H): (List[K], R[H]) = {
          a._1 -> self.PileSyntaxFunctor.pileMap(a._2(), pervious)
        }
      }
      override type QueryType = self.QueryType
      override def pathGen(path: AtomicPath): QueryTranform[QueryType] = self.pathGen(path)
      override def columnGen(tempList: List[QueryType], contentGen: DataPileContentGen): (List[K], () => T) = {
        {
          contentGen.oldDataPiles.flatMap { oldDataPile =>
            oldDataPile.pathWithValues.flatMap { pAndV =>
              val wrap = gen(pAndV.path)
              wrap.gen match {
                case Left(_) => Option.empty[K]
                case Right(tran) =>
                  Option(wrap.apply(tran, pAndV.value.asInstanceOf[AtomicValueImpl[wrap.path.DataType]]))
              }
            }
          }
        } -> { () =>
          self.columnGen(tempList, contentGen)
        }
      }
    }
  }

}

trait SingleFoldableChannel[T, R[_]] extends SingleIOChannel[T, R] with FoldableChannel[T, R] {
  self =>

  val functor: cats.Functor[R]

  override def withFilter[K](gen: AtomicPath => QueryTranform[K]): SingleFoldableChannel[(List[K], () => T), ({ type L[M] = (List[K], R[M]) })#L] = {
    new SingleFoldableChannel[(List[K], () => T), ({ type L[M] = (List[K], R[M]) })#L] {
      override val functor = new Functor[({ type L[M] = (List[K], R[M]) })#L] {
        override def map[A, B](fa: (List[K], R[A]))(f: (A) => B): (List[K], R[B]) = fa._1 -> self.functor.map(fa._2)(f)
      }
      override val PileSyntaxFunctor = new PileSyntaxFunctor[(List[K], () => T), ({ type L[M] = (List[K], R[M]) })#L] {
        override def pileMap[H](a: (List[K], () => T), pervious: DataPileContent => H): (List[K], R[H]) = {
          a._1 -> self.PileSyntaxFunctor.pileMap(a._2(), pervious)
        }
      }
      override type QueryType = self.QueryType
      override def pathGen(path: AtomicPath): QueryTranform[QueryType] = self.pathGen(path)
      override def columnGen(tempList: List[QueryType], contentGen: DataPileContentGen): (List[K], () => T) = {
        {
          contentGen.oldDataPiles.flatMap { oldDataPile =>
            oldDataPile.pathWithValues.flatMap { pAndV =>
              val wrap = gen(pAndV.path)
              wrap.gen match {
                case Left(_) => Option.empty[K]
                case Right(tran) =>
                  Option(wrap.apply(tran, pAndV.value.asInstanceOf[AtomicValueImpl[wrap.path.DataType]]))
              }
            }
          }
        } -> { () =>
          self.columnGen(tempList, contentGen)
        }
      }
    }
  }

}