package net.scalax.fsn.common.atomic

import cats.Applicative
import net.scalax.fsn.core.Atomic

trait FValue[E] extends Atomic[E] {
  val value: E

  override def toString: String = s"FValue(${value.toString})"
}

object FValue {
  implicit def applicativeForEither: Applicative[FValue] = new Applicative[FValue] {
    override def ap[A, B](ff: FValue[A => B])(fa: FValue[A]): FValue[B] = apply(ff.value(fa.value))
    override def pure[A](a: A): FValue[A] = apply(a)
    override def map[A, B](fa: FValue[A])(f: A => B): FValue[B] = apply(f(fa.value))
  }

  def apply[E](value1: E): FValue[E] = new FValue[E] {
    override val value = value1
  }
}