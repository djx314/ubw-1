package net.scalax.ubw.core

object ListUtils {

  /*def splitList[X](spList: List[X], length: Int*): List[List[X]] = {
    val (result, leaveOver) = length.foldLeft(List.empty[List[X]] -> spList) { (a, b) =>
      val (head, tail) = a._2.splitAt(b)
      if (head.size != b) throw new Exception("分离的数组长度不够")
      (head :: a._1) -> tail
    }
    if (!leaveOver.isEmpty) throw new Exception("分离的数组还有剩下的元素")
    result.reverse
  }

  case class WeightData[T](data: List[T], weight: Int)

  case class CatchIndexException(index: Int) extends Exception("已经捕获到 index")*/

  /*def splitWithWeight[X](spList: List[WeightData[X]], length: Int*): List[WeightData[X]] = {
    val (foldResult, leftList) = length.foldLeft(List.empty[WeightData[X]] -> spList) {
      case ((eachData, eachSplit), eachLenght) =>
        val catchIndex = try {
          eachSplit.zipWithIndex.foldLeft(eachLenght) {
            case (leftLength, (model, index)) =>
              if (model.weight == leftLength) {
                throw CatchIndexException(index)
              } else if (model.weight < leftLength) {
                leftLength - model.weight
              } else {
                throw new Exception("剩余的可用长度不足以消费当前元素长度")
              }
          }
        } catch {
          case CatchIndexException(index) => index
          case e: Exception => throw e
        }

        val takeSplit = eachSplit.take(catchIndex + 1)
        val newWeightData = WeightData(takeSplit.flatMap(_.data), takeSplit.map(_.weight).sum)

        (eachData ::: newWeightData :: Nil) -> eachSplit.drop(catchIndex + 1)
    }
    if (!leftList.isEmpty) throw new Exception("分离的数组还有剩下的元素")
    foldResult
  }*/

  /*def splitWithWeight[X](spList: List[WeightData[X]], length: Int*): List[List[WeightData[X]]] = {
    val (foldResult, leftList) = length.foldLeft(List.empty[List[WeightData[X]]] -> spList) {
      case ((eachData, eachSplit), eachLenght) =>
        val catchIndex = try {
          eachSplit.zipWithIndex.foldLeft(eachLenght) {
            case (leftLength, (model, index)) =>
              if (model.weight == leftLength) {
                throw CatchIndexException(index)
              } else if (model.weight < leftLength) {
                leftLength - model.weight
              } else {
                throw new Exception("剩余的可用长度不足以消费当前元素长度")
              }
          }
        } catch {
          case CatchIndexException(index) => index
          case e: Exception => throw e
        }

        val takeSplit = eachSplit.take(catchIndex + 1)

        (eachData ::: takeSplit :: Nil) -> eachSplit.drop(catchIndex + 1)
    }
    if (!leftList.isEmpty) throw new Exception("分离的数组还有剩下的元素")
    foldResult
  }*/

}