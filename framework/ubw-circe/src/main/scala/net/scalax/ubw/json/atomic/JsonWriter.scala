package net.scalax.fsn.json.atomic

import cats.functor.Contravariant
import io.circe.Encoder
import io.circe.syntax._
import net.scalax.fsn.core.FAtomic

trait JsonWriter[E] extends FAtomic[E] {

  type DataType = E
  val writer: Encoder[DataType]

}

object JsonWriter {

  implicit def functorForOption: Contravariant[JsonWriter] = new Contravariant[JsonWriter] {
    override def contramap[A, B](fa: JsonWriter[A])(f: B => A): JsonWriter[B] = new JsonWriter[B] {
      override val writer = {
        fa.writer.contramap(f)
      }
    }
  }

}