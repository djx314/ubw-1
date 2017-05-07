package net.scalax.fsn.core

trait FAtomicPath extends FAtomicWrap

trait FAtomicPathImpl[D] extends FAtomicPath {
  self =>
  override val atomics: List[FAtomic[D]]
  override type DataType = D

  def append(path: FAtomicPathImpl[D]): FAtomicPathImpl[D] = new FAtomicPathImpl[D] {
    override val atomics = self.atomics ::: path.atomics
  }

  def appendAll(path: FAtomicPathImpl[D]*): FAtomicPathImpl[D] = new FAtomicPathImpl[D] {
    override val atomics = self.atomics ::: path.toList.flatMap(_.atomics)
  }

  def appendAtomic(atomic: FAtomic[D]): FAtomicPathImpl[D] = new FAtomicPathImpl[D] {
    override val atomics = self.atomics ::: atomic :: Nil
  }

  def appendAllAtomic(atomics: FAtomic[D]*): FAtomicPathImpl[D] = {
    val atomics1 = atomics
    new FAtomicPathImpl[D] {
      override val atomics = self.atomics ::: atomics1.toList
    }
  }
}

object FAtomicPathImpl {

  def apply[D](atomics: List[FAtomic[D]]): FAtomicPathImpl[D] = {
    val atomics1 = atomics
    new FAtomicPathImpl[D] {
      override val atomics = atomics1
    }
  }

  def empty[D]: FAtomicPathImpl[D] = {
    new FAtomicPathImpl[D] {
      override val atomics = Nil
    }
  }

}