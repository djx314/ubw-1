package net.scalax.fsn.slick.operation
import net.scalax.fsn.core.FColumn
import net.scalax.fsn.slick.atomic.InRetrieve
/*object InRetrieveOperation {

  def filterInRetrieve(columns: List[FColumn]): List[FColumn] = {
    columns.filter { s =>
      FColumn.findOpt(s) { case t: InRetrieve[s.DataType] => t }.map(_.isInRetrieve).getOrElse(true)
    }
  }

}*/