package net.scalax.ubw.database.test

object Test {

  import shapeless._
  object sizeOf extends Poly1 {
    implicit val intCase: Case.Aux[Int, Int] =
      at(s => s * 2)
    implicit val stringCase: Case.Aux[String, String] =
      at(s => s + "2333")
    implicit val booleanCase: Case.Aux[Boolean, Int] =
      at(bool => if (bool) 1 else 0)
  }

  object sizeOf11 extends Poly1 {
    implicit def default[T] = at[T](s => List.empty[String])

    implicit val intCase: Case.Aux[Int, List[String]] =
      at(s => s"整形$s" :: Nil)
    implicit val stringCase: Case.Aux[String, List[String]] =
      at(s => s"字符型$s" :: Nil)
    implicit val booleanCase: Case.Aux[Boolean, List[String]] =
      at(s => s"布尔型$s" :: Nil)
  }

  object Append2 extends Poly2 {
    implicit val caseString = at[List[String], List[String]](_ ++ _)
  }

  val cc = 10 :: "hello" :: (123 :: HNil) :: (233 :: (("abc" -> false) :: HNil) :: HNil) :: (true, "abcdef", 123) :: HNil
  val bb = everywhere(sizeOf)(cc)
  val dd = everything(sizeOf11)(Append2)(cc)
  println(dd)

  case class Person(name: String, skills: List[Skill], hobbbies: List[StemmedString])
  case class Skill(score: Int, title: StemmedString)
  case class StemmedString(original: String, stemmed: String)

  trait TranslatableLP extends Poly1 {
    implicit def default[T] = at[T](_ => Nil: List[String])
  }

  object Translatable extends TranslatableLP {
    implicit def caseStemmedString = at[StemmedString](s => List(s.original))
  }

  object Append extends Poly2 {
    implicit val caseString = at[List[String], List[String]](_ ++ _)
  }

  val strings = everything(Translatable)(Append)(Person("2333", List(Skill(2, StemmedString("ee", "ff")), Skill(3, StemmedString("gg", "hh"))), List(StemmedString("aa", "bb"), StemmedString("cc", "dd"))))

  val strings1 = everywhere(Translatable)(StemmedString("ee", "ff") :: (StemmedString("ee", "ff") :: HNil) :: HNil)

}