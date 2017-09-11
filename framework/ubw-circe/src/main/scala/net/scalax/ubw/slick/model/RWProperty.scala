package net.scalax.ubw.slick.model

case class RWProperty(
  property: String,
  typeName: String,
  inRetrieve: Boolean,
  isAutoInc: Boolean,
  isPrimaryKey: Boolean = false
)