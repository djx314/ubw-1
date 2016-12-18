package net.scalax.fsn.slick.model

case class RWProperty(
  property: String,
  typeName: String,
  inRetrieve: Boolean,
  isAutoInc: Boolean,
  isPrimaryKey: Boolean = false
)