package net.scalax.ubw.extraction.model

import net.scalax.fsn.core.DataPileContent
import net.scalax.ubw.extraction.atomic.Extractor
import net.scalax.ubw.extraction.operation.ExtractorOperation

trait ExtractContent {

  val dataPileContent: DataPileContent
  lazy protected val map: Map[Extractor[_], Any] = dataPileContent.afterWithFilter(ExtractorOperation.extractor) match {
    case list if list.isEmpty =>
      Map.empty[Extractor[_], Any]
    case list =>
      list.reduce(_ ++ _)
  }

  def extract[E](extractor: Extractor[E]): Option[E] = {
    map.get(extractor).map(_.asInstanceOf[E])
  }

  def extractAnyway[E](extractor: Extractor[E]): E = {
    map(extractor).asInstanceOf[E]
  }

}

object ExtractContent {

  def apply(dataPileContent: DataPileContent): ExtractContent = {
    val dataPileContent1 = dataPileContent
    new ExtractContent {
      override val dataPileContent = dataPileContent1
    }
  }

}