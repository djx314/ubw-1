package net.scalax.ubw.core

trait AtomicValue extends AtomicWrap {
  override type DataType
  override type CollType[T] = Option[T]
  override val atomics: Option[Atomic[DataType]]
}

trait AtomicValueImpl[D] extends AtomicValue {
  self =>
  override val atomics: Option[Atomic[D]]
  override type DataType = D

  override def toString = s"AtomicValueImpl($atomics)"
}

object AtomicValueImpl {

  def apply[D](atomics: Option[Atomic[D]]): AtomicValueImpl[D] = {
    val atomics1 = atomics
    new AtomicValueImpl[D] {
      override val atomics = atomics1
    }
  }

  def empty[T]: AtomicValueImpl[T] = new AtomicValueImpl[T] {
    override val atomics = None
    override type DataType = T

    override def equals(obj: Any): Boolean = obj match {
      case elem: AtomicValue if elem.atomics.isEmpty => true
      case _ => false
    }
  }

  //val Zero: AtomicValue = empty[Nothing]

  object Zero {
    def unapply[T](fValue: AtomicValueImpl[T]): Boolean = {
      if (empty[T].equals(fValue)) {
        //Option(())
        true
      } else {
        //None
        false
      }
    }
  }

}