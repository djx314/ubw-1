package net.scalax.ubw.common.atomic

import cats.{ Applicative, Functor }
import net.scalax.ubw.core.Atomic

trait FProperty[E] extends Atomic[E] {
  val proName: String

  override def toString: String = s"FProperty(${proName})"
}

object FProperty {
  implicit val functorForOption: Functor[FProperty] = new Functor[FProperty] {
    def map[A, B](fa: FProperty[A])(f: A => B): FProperty[B] = apply(fa.proName)
  }

  def apply[E](name: String): FProperty[E] = new FProperty[E] {
    override val proName = name
  }
}

trait FDescribe[E] extends Atomic[E] {
  val describe: String

  override def toString: String = s"FDescribe(${describe})"
}
object FDescribe {
  implicit val functorForOption: Functor[FDescribe] = new Functor[FDescribe] {
    def map[A, B](fa: FDescribe[A])(f: A => B): FDescribe[B] = new FDescribe[B] {
      override val describe = fa.describe
    }
  }

  def apply[E](name: String): FDescribe[E] = new FDescribe[E] {
    override val describe = name
  }
}

trait DefaultValue[E] extends Atomic[E] {
  val value: E

  override def toString: String = s"DefaultValue(${value.toString})"
}

object DefaultValue {
  implicit def applicativeForEither: Applicative[DefaultValue] = new Applicative[DefaultValue] {
    override def ap[A, B](ff: DefaultValue[A => B])(fa: DefaultValue[A]): DefaultValue[B] = apply(ff.value(fa.value))

    override def pure[A](a: A): DefaultValue[A] = apply(a)

    override def map[A, B](fa: DefaultValue[A])(f: A => B): DefaultValue[B] = apply(f(fa.value))
  }

  def apply[E](value1: E): DefaultValue[E] = new DefaultValue[E] {
    override val value = value1
  }
}