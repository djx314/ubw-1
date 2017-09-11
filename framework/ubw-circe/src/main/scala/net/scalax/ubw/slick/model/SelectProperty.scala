package net.scalax.ubw.slick.model

case class SelectProperty(
  property: String,
  typeName: String,
  inRetrieve: Boolean,
  canOrder: Boolean,
  isDefaultDesc: Boolean,
  describe: Option[String] = None
)