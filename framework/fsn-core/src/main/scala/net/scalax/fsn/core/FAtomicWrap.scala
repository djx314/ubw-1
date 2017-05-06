package net.scalax.fsn.core

trait FAtomicWrap {
  type DataType
  val atomics: List[FAtomic[DataType]]

  override def toString: String = {
    s"""|paths:
        |${atomics.map(s => "  " + s.toString).mkString("\n")}""".stripMargin
  }
}