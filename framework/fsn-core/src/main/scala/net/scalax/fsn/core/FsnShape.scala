package net.scalax.fsn.core

import scala.language.higherKinds
import shapeless._

trait FsnShape[Packed_, DataType_ /*, TargetType_*/ , UnPacked_[_]] {
  self =>

  type Packed = Packed_
  type UnPacked[T] = UnPacked_[T]
  type DataType = DataType_
  //type TargetType = TargetType_

  def encodeColumn(pile: Packed_): List[FAtomicPath]
  //def decodeColumn(columns: List[FPath]): Packed_
  //def genPiles(pile: Packed_): List[FPile[UnPacked_]]
  //def toTarget(pile: Packed_): TargetType_

  def encodeData(pileData: DataType_): List[UnPacked_[Any]]
  def decodeData(data: List[UnPacked_[Any]]): DataType_

  def zero: DataType_

  val dataLength: Int

  //def packageShape: FsnShape[TargetType_, DataType_, TargetType_, UnPacked_]

}

object FsnShape {

  def hnilFsnShape[T[_]]: FsnShape[HNil, HNil /*, HNil*/ , T] = new FsnShape[HNil, HNil /*, HNil*/ , T] {
    self =>
    override def encodeColumn(pile: HNil): List[FAtomicPath] = Nil
    //override def decodeColumn(columns: List[FPath]): HNil = HNil
    //override def genPiles(pile: HNil): List[FPile[T]] = Nil
    override def encodeData(pileData: HNil): List[T[Any]] = Nil
    override def decodeData(data: List[T[Any]]): HNil = HNil
    //override def toTarget(pile: HNil): HNil = HNil
    override def zero = HNil
    //override def packageShape = self
    override val dataLength = 0
  }

  def fpathFsnShape[T, U[_]](implicit zeroPile: FZeroPile[U[T]]): FsnShape[FAtomicPathImpl[T], U[T] /*, FAtomicPathImpl[T]*/ , U] =
    new FsnShape[FAtomicPathImpl[T], U[T] /*, FAtomicPathImpl[T]*/ , U] {
      self =>
      override def encodeColumn(pile: FAtomicPathImpl[T]): List[FAtomicPath] = pile :: Nil
      //override def decodeColumn(columns: List[FPath]): FAtomicPathImpl[T] = columns.head.asInstanceOf[FAtomicPathImpl[T]]
      /*override def genPiles(pile: FAtomicPathImpl[T]): List[FPile[U]] = {
        FPile[FAtomicPathImpl[T], U[T], FAtomicPathImpl[T], U](pile)(self) :: Nil
        Nil
      }*/
      //override def toTarget(path: FAtomicPathImpl[T]): FAtomicPathImpl[T] = path
      override def encodeData(pileData: U[T]): List[U[Any]] = pileData.asInstanceOf[U[Any]] :: Nil
      override def decodeData(data: List[U[Any]]): U[T] = data.head.asInstanceOf[U[T]]

      override def zero = zeroPile.zero
      //override def packageShape = self

      override val dataLength = 1
    }

  /*implicit def fpileFsnShape[T, U[_]](implicit zeroPile: FZeroPile[U[T]]) /*(implicit fsnShape: FsnShape[FAtomicPathImpl[T], U[T], FAtomicPathImpl[T], U])*/ : FsnShape[FPileImpl[FAtomicPathImpl[T], U[T], U], U[T] /*, FAtomicPathImpl[T]*/ , U] = {
    val fsnShape = implicitly[FsnShape[FAtomicPathImpl[T], U[T] /*, FAtomicPathImpl[T]*/ , U]]

    new FsnShape[FPileImpl[FAtomicPathImpl[T], U[T], U], U[T] /*, FAtomicPathImpl[T]*/ , U] {
      self =>

      override def encodeColumn(pile: FPileImpl[FAtomicPathImpl[T], U[T], U]): List[FAtomicPath] = pile.pathPile :: Nil
      //思路错误，FAtomicPathImpl[T]还有 subs 字段需要特殊处理
      /*override def decodeColumn(columns: List[FPath]): FPileImpl[FAtomicPathImpl[T], U[T], U] = {
        FPile(fsnShape.decodeColumn(columns))(fsnShape)
      }*/
      //override def genPiles(pile: FPileImpl[FAtomicPathImpl[T], U[T], U]): List[FPile[U]] = pile :: Nil
      //override def toTarget(pile: FPileImpl[FAtomicPathImpl[T], U[T], U]): FAtomicPathImpl[T] = pile.pathPile
      override def encodeData(pileData: U[T]): List[U[Any]] = fsnShape.encodeData(pileData)
      override def decodeData(data: List[U[Any]]): U[T] = fsnShape.decodeData(data)

      override def zero = fsnShape.zero
      //override def packageShape = implicitly[FsnShape[FAtomicPathImpl[T], U[T], FAtomicPathImpl[T], U]]
      /*new FsnShape[FAtomicPathImpl[T], U[T], FAtomicPathImpl[T], U] {
        subSelf =>
        override def encodeColumn(pile: FAtomicPathImpl[T]): List[FPath] = pile :: Nil
        //思路错误，FAtomicPathImpl[T]还有 subs 字段需要特殊处理
        /*override def decodeColumn(columns: List[FPath]): FPileImpl[FAtomicPathImpl[T], U[T], U] = {
          FPile(fsnShape.decodeColumn(columns))(fsnShape)
        }*/
        override def genPiles(pile: FAtomicPathImpl[T]): List[FPile[U]] = {
          FPile[FAtomicPathImpl[T], U[T], FAtomicPathImpl[T], U](pile)(subSelf) :: Nil
        }
        override def toTarget(pile: FAtomicPathImpl[T]): FAtomicPathImpl[T] = pile
        override def encodeData(pileData: U[T]): List[U[Any]] = fsnShape.encodeData(pileData)
        override def decodeData(data: List[U[Any]]): U[T] = fsnShape.decodeData(data)

        override def zero = fsnShape.zero
        override def packageShape = subSelf

        override val dataLength = fsnShape.dataLength
      }*/

      override val dataLength = fsnShape.dataLength
    }
  }*/

  /*implicit def fpileFsnShape[U,T <: HList, W, V <: HList, C, D <: HList, A[_]]
  (
    implicit
    subShape: FsnShape[U, W, C, A],
    tailShape: FsnShape[T, V, D, A]
  )
  : FsnShape[FPileImpl[U :: T, W :: V , U], W :: V, FAtomicPathImpl[U :: T], U] = {
    //val fsnShape = implicitly[FsnShape[FAtomicPathImpl[U :: T], W :: V, FAtomicPathImpl[U :: T], U]]

    new FsnShape[FPileImpl[U :: T, W :: V , U], W :: V, FAtomicPathImpl[U :: T], U] {
      self =>

      override def encodeColumn(pile: FPileImpl[U :: T, W :: V , U]): List[FPath] = {
        val headPile :: tailPile = pile.pathPile
        subShape.encodeColumn(headPile) ::: tailShape.encodeColumn(tailPile)
      }
      //思路错误，FAtomicPathImpl[T]还有 subs 字段需要特殊处理
      /*override def decodeColumn(columns: List[FPath]): FPileImpl[FAtomicPathImpl[T], U[T], U] = {
        FPile(fsnShape.decodeColumn(columns))(fsnShape)
      }*/
      override def genPiles(pile: FPileImpl[U :: T, W :: V , U]): List[FPile[U]] = pile :: Nil
      override def toTarget(pile: FPileImpl[U :: T, W :: V , U]): FAtomicPathImpl[U :: T] = pile.pathPile
      override def encodeData(pileData: U[T]): List[U[Any]] = fsnShape.encodeData(pileData)
      override def decodeData(data: List[U[Any]]): U[T] = fsnShape.decodeData(data)

      override def zero = subShape.zero :: tailShape.zero
      override def packageShape = new FsnShape[FAtomicPathImpl[T], U[T], FAtomicPathImpl[T], U] {
        subSelf =>
        override def encodeColumn(pile: FAtomicPathImpl[T]): List[FPath] = pile :: Nil
        //思路错误，FAtomicPathImpl[T]还有 subs 字段需要特殊处理
        /*override def decodeColumn(columns: List[FPath]): FPileImpl[FAtomicPathImpl[T], U[T], U] = {
          FPile(fsnShape.decodeColumn(columns))(fsnShape)
        }*/
        override def genPiles(pile: FAtomicPathImpl[T]): List[FPile[U]] = {
          FPile[FAtomicPathImpl[T], U[T], FAtomicPathImpl[T], U](pile)(subSelf) :: Nil
        }
        override def toTarget(pile: FAtomicPathImpl[T]): FAtomicPathImpl[T] = pile
        override def encodeData(pileData: U[T]): List[U[Any]] = fsnShape.encodeData(pileData)
        override def decodeData(data: List[U[Any]]): U[T] = fsnShape.decodeData(data)

        override def zero = subShape.packageShape.zero :: tailShape.packageShape.zero
        override def packageShape = subSelf

        override val dataLength = subShape.packageShape.dataLength + tailShape.packageShape.dataLength
      }

      override val dataLength = subShape.dataLength + tailShape.dataLength
    }
  }*/

  /*implicit def hlistFsnShape[T <: HList, U, W, V <: HList, C, D <: HList, A[_]](
    implicit
    subShape: FsnShape[U, W, C, A],
    tailShape: FsnShape[T, V, D, A]
  ): FsnShape[U :: T, W :: V, C :: D, A] = new FsnShape[U :: T, W :: V, C :: D, A] {
    self =>

    override def encodeColumn(pile: U :: T): List[FAtomicPath] = {
      val headPile :: tailPile = pile
      subShape.encodeColumn(headPile) ::: tailShape.encodeColumn(tailPile)
    }
    /*override def decodeColumn(columns: List[FPath]): U :: T = {
      subShape.decodeColumn(columns.take(subShape.dataLength)) :: tailShape.decodeColumn(columns.drop(subShape.dataLength))
    }*/
    override def genPiles(pile: U :: T): List[FPile[A]] = {
      val headPile :: tailPile = pile
      subShape.genPiles(headPile) ::: tailShape.genPiles(tailPile)
    }
    override def encodeData(pileData: W :: V): List[A[Any]] = {
      val headData :: tailData = pileData
      subShape.encodeData(headData) ::: tailShape.encodeData(tailData)
    }
    override def toTarget(pile: U :: T): C :: D = {
      val subPile :: tailPile = pile
      subShape.toTarget(subPile) :: tailShape.toTarget(tailPile)
    }

    override def decodeData(data: List[A[Any]]): W :: V = {
      subShape.decodeData(data.take(subShape.dataLength)) :: tailShape.decodeData(data.drop(subShape.dataLength))
    }
    override def packageShape = new FsnShape[C :: D, W :: V, C :: D, A] {
      subSelf =>

      override def encodeColumn(pile: C :: D): List[FAtomicPath] = {
        val headPile :: tailPile = pile
        subShape.packageShape.encodeColumn(headPile) ::: tailShape.packageShape.encodeColumn(tailPile)
      }
      /*override def decodeColumn(columns: List[FPath]): U :: T = {
        subShape.decodeColumn(columns.take(subShape.dataLength)) :: tailShape.decodeColumn(columns.drop(subShape.dataLength))
      }*/
      override def genPiles(pile: C :: D): List[FPile[A]] = {
        val headPile :: tailPile = pile
        subShape.packageShape.genPiles(headPile) ::: tailShape.packageShape.genPiles(tailPile)
      }
      override def encodeData(pileData: W :: V): List[A[Any]] = {
        val headData :: tailData = pileData
        subShape.packageShape.encodeData(headData) ::: tailShape.packageShape.encodeData(tailData)
      }
      override def toTarget(pile: C :: D): C :: D = {
        pile
      }

      override def decodeData(data: List[A[Any]]): W :: V = {
        subShape.packageShape.decodeData(data.take(subShape.dataLength)) :: tailShape.packageShape.decodeData(data.drop(subShape.dataLength))
      }
      override def packageShape = subSelf

      override def zero = subShape.packageShape.zero :: tailShape.packageShape.zero

      override val dataLength = subShape.packageShape.dataLength + tailShape.packageShape.dataLength
    }

    override def zero = subShape.zero :: tailShape.zero

    override val dataLength = subShape.dataLength + tailShape.dataLength
  }*/

}