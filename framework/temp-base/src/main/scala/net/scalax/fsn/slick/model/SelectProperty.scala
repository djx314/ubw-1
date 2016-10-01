package net.scalax.fsn.slick.model

case class SelectProperty(
  property: String,
  typeName: String,
  inRetrieve: Boolean,
  canOrder: Boolean,
  isDefaultDesc: Boolean
)