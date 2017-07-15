package net.scalax.fsn.common.atomic

import cats.Applicative
import net.scalax.fsn.core.FAtomic

import scala.concurrent.{ ExecutionContext, Future }

trait FFutureValue[E] extends FAtomic[E] {
  self =>

  val value: Future[E]

  override def toString: String = s"FFutureValue(${value.toString})"
}

object FFutureValue {
  implicit def applicativeForEither(implicit ec: ExecutionContext): Applicative[FFutureValue] = new Applicative[FFutureValue] {
    override def ap[A, B](ff: FFutureValue[A => B])(fa: FFutureValue[A]): FFutureValue[B] = apply(ff.value.flatMap(t => fa.value.map(u => t(u))))

    override def pure[A](a: A): FFutureValue[A] = apply(Future successful a)

    override def map[A, B](fa: FFutureValue[A])(f: A => B): FFutureValue[B] = apply(fa.value.map(f))
  }

  def apply[E](fValue: Future[E]): FFutureValue[E] = new FFutureValue[E] {
    override val value = fValue
  }
}