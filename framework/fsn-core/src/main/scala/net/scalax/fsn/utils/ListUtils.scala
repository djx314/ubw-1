package net.scalax.fsn.core

object ListUtils {

  def splitList[X](spList: List[X], length: Int*): List[List[X]] = {
    val (result, leaveOver) = length.foldLeft(List.empty[List[X]] -> spList) { (a, b) =>
      val (head, tail) = a._2.splitAt(b)
      if (head.size != b) throw new Exception("分离的数组长度不够")
      (head :: a._1) -> tail
    }
    if (! leaveOver.isEmpty) throw new Exception("分离的数组还有剩下的元素")
    result.reverse
  }

}