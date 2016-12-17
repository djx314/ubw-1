package net.scalax.fsn.parameter.atomic

import io.circe.{Decoder, HCursor}
import io.circe.generic.auto._

sealed trait FParam[T] {
  def data: T
  def paramComp: FCompAbs
}

object FParam {

  implicit def decodeFParam[T](implicit decoder: Decoder[T]): Decoder[FParam[T]] = new Decoder[FParam[T]] {
    final def apply(c: HCursor): Decoder.Result[FParam[T]] = {
      c.as[FInParam[T]]
    }
  }/*new Decoder[FParam[T]] {
    case class TempDecoder(FInParam: FInParam[T])
    final def apply(c: HCursor): Decoder.Result[FParam[T]] = {
      c.as[TempDecoder].map(_.FInParam)
    }
  }*/

}

trait FComponent[T] extends FParam[T] {

  override def data = throw new Exception("data 函数只能在前端返回数据在析构以后才能使用")

  val paramComp: FCompAbs

}

object FComponent {

  def gen[T](paramComp1: FCompAbs) = new FComponent[T] {
    override val paramComp = paramComp1
  }

}

case class FInParam[T](
  override val data: T
) extends FParam[T] {
  override def paramComp: FCompAbs = throw new Exception("该实现没有提供组件信息")
}
/*object FInParam {

  implicit def decodeFIParam[T](implicit decoder: Decoder[T]) = {
    implicitly[Decoder[FInParam[T]]]
  }/*new Decoder[FInParam[T]] {
    final def apply(c: HCursor): Decoder.Result[FInParam[T]] = {
      implicit val jsonEncoder = implicitly[Decoder[FInParam[T]]]
      c.as[FInParam[T]](jsonEncoder)
    }
  }*/

}*/