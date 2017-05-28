package net.scalax.fsn.common.atomic

import cats.Applicative
import net.scalax.fsn.core.FAtomic

trait FValue[E] extends FAtomic[E] {
  val value: E

  override def toString: String = s"FValue(${value.toString})"
}

object FValue {
  implicit def applicativeForEither: Applicative[FValue] = new Applicative[FValue] {
    override def ap[A, B](ff: FValue[A => B])(fa: FValue[A]): FValue[B] = new FValue[B] {
      override val value = ff.value(fa.value)
    }

    override def pure[A](a: A): FValue[A] = new FValue[A] {
      override val value = a
    }

    override def map[A, B](fa: FValue[A])(f: A => B): FValue[B] = new FValue[B] {
      override val value = f(fa.value)
    }
  }
}