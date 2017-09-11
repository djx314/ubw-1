package net.scalax.ubw.validate.atomic

import net.scalax.ubw.core.Atomic

import scala.concurrent.Future

trait ValidatorF[E] extends Atomic[E] {

  type DataType = E
  def validateF(proName: String, describe: String): PartialFunction[Option[DataType], Future[Option[ErrorMessage]]]
}

trait Validator[E] extends ValidatorF[E] {

  override type DataType = E

  def validate(proName: String, describe: String): PartialFunction[Option[DataType], Option[ErrorMessage]]

  override def validateF(proName: String, describe: String): PartialFunction[Option[DataType], Future[Option[ErrorMessage]]] = {
    {
      case data: Option[DataType] if validate(proName, describe).isDefinedAt(data) =>
        Future successful validate(proName, describe).apply(data)
    }
  }

}

case class ErrorMessage(property: String, message: String, level: String)

object ErrorMessage {

  val errorType = "error"
  val warnType = "warn"

  def error(property: String, message: String): ErrorMessage = {
    ErrorMessage(property, message, errorType)
  }

  def warn(property: String, message: String): ErrorMessage = {
    ErrorMessage(property, message, warnType)
  }

}