package net.scalax.fsn.core

trait FPath {
  type DataType
  val atomics: List[FAtomic[DataType]]

  override def toString: String = {
    s"""|paths:
       |${ atomics.map(s => "  " + s.toString).mkString("\n") }""".stripMargin
  }
}

case class FPathImpl[D](override val atomics: List[FAtomic[D]]) extends FPath {
  override type DataType = D
}