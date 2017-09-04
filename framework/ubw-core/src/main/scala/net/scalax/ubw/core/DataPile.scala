package net.scalax.fsn.core

sealed abstract trait DataPile {
  self =>
  type DataType
  val data: DataType

  def replaceData(newData: DataType): DataPile
  def renderFromList(newData: List[AtomicValue]): (DataPile, List[AtomicValue])
  def toAtomicValues(data: DataType): List[AtomicValue]
  def selfPaths: List[AtomicPath]
}

trait DataPileList extends DataPile {
  self =>

  type PileType
  override type DataType

  val pileEntity: PileType

  def encodePiles: List[CommonDataPile]
  def decodePileData(data: List[Any]): DataType
  def encodePileData(data: DataType): List[Any]

  override def replaceData(newData: DataType): DataPileList
  override def renderFromList(newData: List[AtomicValue]): (DataPileList, List[AtomicValue])
  override def toAtomicValues(data: DataType): List[AtomicValue] = {
    encodePileData(data).zip(encodePiles).flatMap {
      case (eachData, eachPile) =>
        eachPile.fShape.encodeData(eachData.asInstanceOf[eachPile.DataType])
    }
  }
  override def selfPaths: List[AtomicPath] = {
    self.encodePiles.flatMap(_.selfPaths)
  }
}

class DataPileListImpl[PT, DT](
    override val pileEntity: PT,
    override val data: DT,
    encoder: List[CommonDataPile],
    dataDecoder: List[Any] => DT,
    dataEncoder: DT => List[Any]
) extends DataPileList {
  override type PileType = PT
  override type DataType = DT

  override def encodePiles: List[CommonDataPile] = encoder
  override def decodePileData(data: List[Any]): DT = dataDecoder(data)
  override def encodePileData(data: DataType): List[Any] = dataEncoder(data)

  override def replaceData(newData: DataType): DataPileListImpl[PT, DT] = {
    /*new DataPileListImpl(
      pileEntity,
      newData,
      encoder,
      decoder,
      dataDecoder
    )*/
    ???
  }

  override def renderFromList(newData: List[AtomicValue]): (DataPileListImpl[List[CommonDataPile], DT], List[AtomicValue]) = {
    val (commonPileList, resultData) = encodePiles.foldLeft(List.empty[CommonDataPile] -> newData) {
      case ((currentPileList, currentData), currentPile) =>
        val (pile, data) = currentPile.renderFromList(currentData)
        (currentPileList ::: pile :: Nil) -> data
    }
    new DataPileListImpl(
      //decodePiles(commonPileList),
      commonPileList,
      decodePileData(commonPileList.map(_.data)),
      commonPileList,
      //decoder,
      dataDecoder,
      dataEncoder
    ) -> resultData
  }
}

abstract trait CommonDataPile extends DataPile {
  self =>

  type PathType
  override type DataType

  val pathPile: PathType
  val fShape: PileShape[PathType, DataType]

  override def replaceData(newData: DataType): CommonDataPile
  override def renderFromList(newData: List[AtomicValue]): (CommonDataPile, List[AtomicValue])

  override def toAtomicValues(data: DataType): List[AtomicValue] = {
    fShape.encodeData(data)
  }
  override def selfPaths: List[AtomicPath] = {
    self.fShape.encodeColumn(self.pathPile)
  }
}

trait BranchDataPile extends CommonDataPile {
  self =>

  val subs: DataPile
  def dataFromSub(subDatas: Any): DataType
  override def replaceData(newData: DataType): BranchDataPile
  override def renderFromList(newData: List[AtomicValue]): (BranchDataPile, List[AtomicValue])

}

class BranchDataPileImpl[PT, DT](
    override val pathPile: PT,
    override val fShape: PileShape[PT, DT],
    override val subs: DataPile,
    override val data: DT,
    dataFromSubFunc: Any => DT
) extends BranchDataPile {
  override type PathType = PT
  override type DataType = DT
  override def dataFromSub(subDatas: Any): DataType = dataFromSubFunc(subDatas)
  override def replaceData(newData: DataType): BranchDataPileImpl[PT, DT] = {
    new BranchDataPileImpl(
      pathPile,
      fShape,
      subs,
      newData,
      dataFromSubFunc
    )
  }
  override def renderFromList(newData: List[AtomicValue]): (BranchDataPileImpl[PT, DT], List[AtomicValue]) = {
    val (headList, tailList) = newData.splitAt(fShape.dataLength)
    replaceData(fShape.decodeData(headList)) -> tailList
  }
}

trait LeafDataPile extends CommonDataPile {
  self =>
  override def replaceData(newData: DataType): LeafDataPile
  override def renderFromList(newData: List[AtomicValue]): (LeafDataPile, List[AtomicValue])
}

class LeafDataPileImpl[PT, DT](
    override val pathPile: PT,
    override val data: DT,
    override val fShape: PileShape[PT, DT]
) extends LeafDataPile {
  override type PathType = PT
  override type DataType = DT

  override def replaceData(newData: DataType): LeafDataPileImpl[PT, DT] = {
    new LeafDataPileImpl(
      pathPile,
      newData,
      fShape
    )
  }
  override def renderFromList(newData: List[AtomicValue]): (LeafDataPileImpl[PT, DT], List[AtomicValue]) = {
    val (headList, tailList) = newData.splitAt(fShape.dataLength)
    replaceData(fShape.decodeData(headList)) -> tailList
  }
}

object DataPile {

  case class TransPileWrap(root: Pile, drops: List[Pile])
  type TransResult[T] = Either[AtomicException, T]

  def genTreeTailCall[U](pathGen: AtomicPath => QueryTranform[U], oldPile: Pile, newPile: Pile): TransResult[TransPileWrap] = {
    oldPile -> newPile match {
      case (commonPile: CommonPile, leafPile: LeafPile) =>
        val transforms = leafPile.fShape.encodeColumn(leafPile.pathPile).map(pathGen)
        if (transforms.forall(_.gen.isRight)) {
          Right(TransPileWrap(newPile, List(commonPile)))
        } else {
          Left(AtomicException(transforms.map(_.gen).collect { case Left(AtomicException(s)) => s }.flatten))
        }

      case (oldPile: BranchPile, newPile: BranchPile) =>
        genTreeTailCall(pathGen, oldPile.subs, newPile.subs) match {
          case Left(_) =>
            genTreeTailCall(pathGen, oldPile, new LeafPileImpl(
              newPile.pathPile, newPile.fShape
            ))
          case Right(TransPileWrap(newSubResultPile, pileList)) =>
            Right(TransPileWrap(new BranchPileImpl(
              newPile.pathPile,
              newPile.fShape,
              newSubResultPile,
              newPile.dataFromSub _
            ), pileList))
        }

      case (oldPile: PileList, newPile: PileList) =>
        val newPiles = newPile.encodePiles
        val oldPiles = oldPile.encodePiles
        val listResult = oldPiles.zip(newPiles).map {
          case (oldP, newP) =>
            genTreeTailCall(pathGen, oldP, newP)
        }
        val isSuccess = listResult.forall(_.isRight)
        if (isSuccess) {
          val (newPiles, newPileList) = listResult.map {
            case Right(TransPileWrap(root, drops)) => root -> drops
            case _ => throw new IllegalArgumentException("不可识别的输入")
          }.unzip
          Right(TransPileWrap(new PileListImpl[List[Any], newPile.DataType](
            newPiles.map(_.asInstanceOf[CommonPile]),
            newPiles.map(_.asInstanceOf[CommonPile]),
            newPile.decodePileData _,
            newPile.encodePileData _
          ), newPileList.flatten))
        } else {
          Left(listResult.collect {
            case Left(ex) => ex
            case _ => throw new IllegalArgumentException("不可识别的输入")
          }.reduce((a1, a2) =>
            AtomicException(a1.typeTags ::: a2.typeTags)))
        }
      case _ => throw new IllegalArgumentException("不可识别的输入")
    }
  }

  def genTree[U](pathGen: AtomicPath => QueryTranform[U], pile: Pile): TransResult[TransPileWrap] = {
    genTreeTailCall(pathGen, pile, pile)
  }

  def transformTree[U, T](pathGen: AtomicPath => QueryTranform[U])(columnGen: (List[U], List[AtomicValue] => List[DataPile]) => T): PileSyntax1111.PileGen[T] = new PileSyntax1111.PileGen[T] {
    override def gen(prePiles: Pile): Either[AtomicException, PileSyntax1111.PilePip[T]] = {
      val piles = prePiles

      val calculatePiles =
        genTree(pathGen, piles)

      calculatePiles.right.map {
        case TransPileWrap(newPiles, summaryPiles) =>
          PileSyntax1111.PilePipImpl(piles = newPiles, valueFunc = { dataPiles: List[DataPile] =>
            val (oldDataPile, _) = summaryPiles.foldLeft(List.empty[DataPile], dataPiles.map(_.data.asInstanceOf[Any])) {
              case ((dataPileList, data), pile) =>
                val (currentDataPile, remainData) = fromPile(pile, data)
                (dataPileList ::: currentDataPile :: Nil) -> remainData
            }

            val result = oldDataPile.map { pile =>
              val pathWithValue = pile.selfPaths.zip(pile.toAtomicValues(pile.data))
              pathWithValue.map {
                case (path, value) =>
                  val transform = pathGen(path)
                  transform.apply(transform.gen.right.get, value.asInstanceOf[AtomicValueImpl[transform.path.DataType]])
              }
            }.flatten

            val atomicValueGen = { newAtomicValues: List[AtomicValue] =>
              oldDataPile.foldLeft(List.empty[DataPile] -> newAtomicValues) {
                case ((newDataPiles, currentValues), oldDataPile) =>
                  val (newDataPile, remainData) = oldDataPile.renderFromList(currentValues)
                  (newDataPiles ::: newDataPile :: Nil) -> remainData
              }._1
            }

            columnGen(result, atomicValueGen)
          })
      }

    }
  }

  def fromPile(pile: Pile, data: List[Any]): (DataPile, List[Any]) = {
    pile match {
      case leaf: LeafPile =>
        val head :: tail = data
        new LeafDataPileImpl(
          pathPile = leaf.pathPile,
          data = head.asInstanceOf[leaf.DataType],
          fShape = leaf.fShape
        ) -> tail
      case branch: BranchPile =>
        val (subDataPile, tailList) = fromPile(branch.subs, data)
        new BranchDataPileImpl(
          pathPile = branch.pathPile,
          fShape = branch.fShape,
          subs = subDataPile,
          data = branch.dataFromSub(subDataPile.data),
          dataFromSubFunc = branch.dataFromSub _
        ) -> tailList
      case listPile: PileList =>
        val (subDataPileList, remainList) = listPile.encodePiles.foldLeft(List.empty[DataPile] -> data) {
          case ((dataPiles, listData), eachSubPile) =>
            val (eachDataPile, tailData) = fromPile(eachSubPile, listData)
            (dataPiles ::: eachDataPile :: Nil) -> tailData
        }
        new DataPileListImpl[List[Any], listPile.DataType](
          pileEntity = subDataPileList,
          data = listPile.decodePileData(subDataPileList.map(_.data)),
          encoder = subDataPileList.map(_.asInstanceOf[CommonDataPile]),
          //decoder: List[CommonDataPile] => PT,
          dataDecoder = listPile.decodePileData _,
          dataEncoder = listPile.encodePileData _
        ) -> remainList
    }
  }

}