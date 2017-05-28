package net.scalax.fsn.common.atomic

import cats.{ Applicative, Functor }
import net.scalax.fsn.core.FAtomic

trait FProperty[E] extends FAtomic[E] {
  val proName: String

  override def toString: String = s"FProperty(${proName})"
}

object FProperty {
  implicit val functorForOption: Functor[FProperty] = new Functor[FProperty] {
    def map[A, B](fa: FProperty[A])(f: A => B): FProperty[B] = new FProperty[B] {
      override val proName = fa.proName
    }
  }
}

trait FDescribe[E] extends FAtomic[E] {
  val describe: String

  override def toString: String = s"FDescribe(${describe})"
}
object FDescribe {
  implicit val functorForOption: Functor[FDescribe] = new Functor[FDescribe] {
    def map[A, B](fa: FDescribe[A])(f: A => B): FDescribe[B] = new FDescribe[B] {
      override val describe = fa.describe
    }
  }
}

trait DefaultValue[E] extends FAtomic[E] {
  val value: E

  override def toString: String = s"DefaultValue(${value.toString})"
}

object DefaultValue {
  implicit def applicativeForEither: Applicative[DefaultValue] = new Applicative[DefaultValue] {
    override def ap[A, B](ff: DefaultValue[A => B])(fa: DefaultValue[A]): DefaultValue[B] = new DefaultValue[B] {
      override val value = ff.value(fa.value)
    }

    override def pure[A](a: A): DefaultValue[A] = new DefaultValue[A] {
      override val value = a
    }

    override def map[A, B](fa: DefaultValue[A])(f: A => B): DefaultValue[B] = new DefaultValue[B] {
      override val value = f(fa.value)
    }
  }
}