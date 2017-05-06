package net.scalax.fsn.core

trait FAtomicValue extends FAtomicWrap

trait FAtomicValueImpl[D] extends FAtomicValue {
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

object FAtomicValueImpl {

  def apply[D](atomics: List[FAtomic[D]]): FAtomicValueImpl[D] = {
    val atomics1 = atomics
    new FAtomicValueImpl[D] {
      override val atomics = atomics1
    }
  }

  def empty[D]: FAtomicValueImpl[D] = {
    new FAtomicValueImpl[D] {
      override val atomics = Nil
    }
  }

}