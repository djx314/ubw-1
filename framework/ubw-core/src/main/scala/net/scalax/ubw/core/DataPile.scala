package net.scalax.ubw.core

sealed abstract trait DataPile {
  self =>
  type DataType
  val data: DataType

  def replaceData(newData: DataType): DataPile
  def renderFromList(newData: List[AtomicValue]): (DataPile, List[AtomicValue])
  def toAtomicValues(data: DataType): List[AtomicValue]
  def selfPaths: List[AtomicPath]
  def pathWithValues: List[PathWithValues]
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
  override def pathWithValues: List[PathWithValues] = {
    encodePiles.flatMap(_.pathWithValues)
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
    new DataPileListImpl(
      pileEntity,
      newData,
      encoder,
      dataDecoder,
      dataEncoder
    )
  }

  override def renderFromList(newData: List[AtomicValue]): (DataPileListImpl[PT, DT], List[AtomicValue]) = {
    val (commonPileList, resultData) = encodePiles.foldLeft(List.empty[CommonDataPile] -> newData) {
      case ((currentPileList, currentData), currentPile) =>
        val (pile, data) = currentPile.renderFromList(currentData)
        (currentPileList ::: pile :: Nil) -> data
    }
    replaceData(decodePileData(commonPileList.map(_.data))) -> resultData
  }

  /*override def renderFromList(newData: List[AtomicValue]): (DataPileListImpl[List[CommonDataPile], DT], List[AtomicValue]) = {
    val (commonPileList, resultData) = encodePiles.foldLeft(List.empty[CommonDataPile] -> newData) {
      case ((currentPileList, currentData), currentPile) =>
        val (pile, data) = currentPile.renderFromList(currentData)
        (currentPileList ::: pile :: Nil) -> data
    }
    new DataPileListImpl(
      commonPileList,
      decodePileData(commonPileList.map(_.data)),
      commonPileList,
      dataDecoder,
      dataEncoder
    ) -> resultData
  }*/
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
  override def pathWithValues: List[PathWithValues] = {
    subs.pathWithValues ::: fShape.encodeColumn(pathPile).zip(fShape.encodeData(data)).map { s =>
      new PathWithValues {
        valueSelf =>
        override type DataType = s._1.DataType
        override val path = s._1.asInstanceOf[AtomicPathImpl[valueSelf.DataType]]
        override val value = s._2.asInstanceOf[AtomicValueImpl[valueSelf.DataType]]
      }
    }
  }

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
  override def pathWithValues: List[PathWithValues] = {
    fShape.encodeColumn(pathPile).zip(fShape.encodeData(data)).map { s =>
      new PathWithValues {
        valueSelf =>
        override type DataType = s._1.DataType
        override val path = s._1.asInstanceOf[AtomicPathImpl[valueSelf.DataType]]
        override val value = s._2.asInstanceOf[AtomicValueImpl[valueSelf.DataType]]
      }
    }
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

  def transformTree[U, T](pathGen: AtomicPath => QueryTranform[U])(columnGen: (List[U], DataPileContentGen) => T): SingleInputChannel[T] = {
    /*val pileGen1 = new Channel.PileGen[T] {
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
    new InputChannel[T] {
      override val pilesGen = pileGen1
    }*/
    val columnGen1 = columnGen
    val pathGen1 = pathGen
    new SingleInputChannel[T] {
      override def columnGen(tempList: List[QueryType], contentGen: DataPileContentGen): T = columnGen1(tempList, contentGen)
      override def pathGen(path: AtomicPath): QueryTranform[QueryType] = pathGen1(path)
      override type QueryType = U
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
          dataDecoder = listPile.decodePileData _,
          dataEncoder = listPile.encodePileData _
        ) -> remainList
    }
  }

}