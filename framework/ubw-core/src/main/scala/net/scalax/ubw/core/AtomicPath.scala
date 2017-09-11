package net.scalax.ubw.core

trait AtomicPath extends AtomicWrap {
  override type DataType
  override type CollType[T] = List[T]
  override val atomics: List[Atomic[DataType]]

  override def toString: String = {
    s"""|paths:
        |${atomics.map(s => "  " + s.toString).mkString("\n")}""".stripMargin
  }
}

trait AtomicPathImpl[D] extends AtomicPath {
  self =>
  override val atomics: List[Atomic[D]]
  override type DataType = D

  def append(path: AtomicPathImpl[D]): AtomicPathImpl[D] = new AtomicPathImpl[D] {
    override val atomics = self.atomics ::: path.atomics
  }

  def appendAll(path: AtomicPathImpl[D]*): AtomicPathImpl[D] = new AtomicPathImpl[D] {
    override val atomics = self.atomics ::: path.toList.flatMap(_.atomics)
  }

  def appendAtomic(atomic: Atomic[D]): AtomicPathImpl[D] = new AtomicPathImpl[D] {
    override val atomics = self.atomics ::: atomic :: Nil
  }

  def appendAllAtomic(atomics: Atomic[D]*): AtomicPathImpl[D] = {
    val atomics1 = atomics
    new AtomicPathImpl[D] {
      override val atomics = self.atomics ::: atomics1.toList
    }
  }
}

object AtomicPathImpl {

  def apply[D](atomics: List[Atomic[D]]): AtomicPathImpl[D] = {
    val atomics1 = atomics
    new AtomicPathImpl[D] {
      override val atomics = atomics1
    }
  }

  def empty[D]: AtomicPathImpl[D] = {
    new AtomicPathImpl[D] {
      override val atomics = Nil
    }
  }

}