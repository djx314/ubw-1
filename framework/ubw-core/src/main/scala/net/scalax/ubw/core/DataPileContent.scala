package net.scalax.fsn.core

trait DataPileContentGen {
  self =>

  val oldDataPiles: List[DataPile]
  val previousContent: Option[DataPileContent]
  def atomicValueGen(atomicList: List[AtomicValue]): List[DataPile]

  def toContent(atomicList: => List[AtomicValue]): DataPileContent = {
    lazy val atomicList1 = atomicList
    lazy val newDataPiles1 = atomicValueGen(atomicList1)
    new DataPileContent {
      override lazy val atomicList = atomicList1
      override lazy val oldDataPiles = self.oldDataPiles
      override lazy val newDataPiles = newDataPiles1
      override lazy val previousContent = self.previousContent
    }
  }
}

trait DataPileContent {
  val atomicList: List[AtomicValue]
  val oldDataPiles: List[DataPile]
  val newDataPiles: List[DataPile]
  val previousContent: Option[DataPileContent]
}