package net.scalax.ubw.core

trait PathWithValues {

  type DataType
  val path: AtomicPathImpl[DataType]
  val value: AtomicValueImpl[DataType]

}