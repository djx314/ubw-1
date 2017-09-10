package net.scalax.fsn.core

trait PathWithValues {

  type DataType
  val path: AtomicPathImpl[DataType]
  val value: AtomicValueImpl[DataType]

}