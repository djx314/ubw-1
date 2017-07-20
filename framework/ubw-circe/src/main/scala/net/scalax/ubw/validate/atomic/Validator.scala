package net.scalax.ubw.validate.atomic

import net.scalax.fsn.core.FAtomic

trait Validator[E] extends FAtomic[E] {

  type DataType = E

  def validate(proName: String): PartialFunction[Option[DataType], Option[ErrorMessage]]

}

case class ErrorMessage(mess: String)