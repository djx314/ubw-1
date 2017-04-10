package net.scalax.fsn.core

trait FPath {
  type DataType
  val atomics: List[FAtomic[DataType]]

  override def toString: String = {
    s"""|paths:
       |${atomics.map(s => "  " + s.toString).mkString("\n")}""".stripMargin
  }
}

trait FPathImpl[D] extends FPath {
  self =>
  override val atomics: List[FAtomic[D]]
  override type DataType = D

  def append(path: FPathImpl[D]): FPathImpl[D] = new FPathImpl[D] {
    override val atomics = self.atomics ::: path.atomics
  }

  def appendAll(path: FPathImpl[D]*): FPathImpl[D] = new FPathImpl[D] {
    override val atomics = self.atomics ::: path.toList.flatMap(_.atomics)
  }

  def appendAtomic(atomic: FAtomic[D]): FPathImpl[D] = new FPathImpl[D] {
    override val atomics = self.atomics ::: atomic :: Nil
  }

  def appendAllAtomic(atomics: FAtomic[D]*): FPathImpl[D] = {
    val atomics1 = atomics
    new FPathImpl[D] {
      override val atomics = self.atomics ::: atomics1.toList
    }
  }
}

object FPathImpl {

  def apply[D](atomics: List[FAtomic[D]]): FPathImpl[D] = {
    val atomics1 = atomics
    new FPathImpl[D] {
      override val atomics = atomics1
    }
  }

}