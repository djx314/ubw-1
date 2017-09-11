package net.scalax.ubw.extraction.model

import net.scalax.ubw.core.DataPileContent
import net.scalax.ubw.extraction.atomic.Extractor

trait ExtractContent {

  //val dataPileContent: DataPileContent
  protected val map: Map[Extractor[_], Any] /*= dataPileContent.afterWithFilter(ExtractorOperation.extractor) match {
    case list if list.isEmpty =>
      Map.empty[Extractor[_], Any]
    case list =>
      list.reduce(_ ++ _)
  }*/

  def extract[E](extractor: Extractor[E]): Option[E] = {
    map.get(extractor).map(_.asInstanceOf[E])
  }

  def extractAnyway[E](extractor: Extractor[E]): E = {
    map(extractor).asInstanceOf[E]
  }

}

object ExtractContent {

  def apply(map: Map[Extractor[_], Any]): ExtractContent = {
    val map1 = map
    new ExtractContent {
      override val map = map1
    }
  }

}