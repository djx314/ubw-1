package net.scalax.fsn.database.test

import net.scalax.fsn.core._
import org.scalatest._
import org.scalatest.concurrent.ScalaFutures
import net.scalax.fsn.json.atomic.{JsonReader, JsonWriter}
import net.scalax.fsn.slick.atomic.SlickCreate
import net.scalax.fsn.mix.helpers.{In, SlickCRUDImplicits}
import shapeless._
import io.circe._
import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._
import net.scalax.fsn.common.atomic.{DefaultValue, FProperty}
import scala.language.implicitConversions

class ParTest extends FlatSpec
  with Matchers
  with EitherValues
  with ScalaFutures
  with BeforeAndAfterAll
  with BeforeAndAfter
  with FAtomicGenHelper
  with FAtomicShapeHelper
  with PilesPolyHelper {

  "shapes" should "find readers in Atomic in FPath" in {
    val path = FPathImpl(In.jRead[Long] ::: In.jWrite[Long])
    val bb: FAtomicGen[JsonReader] = needAtomic[JsonReader]
    val jsonReaderGen: AbstractFAtomicGen = needAtomic[JsonReader]

    //println(bb.gen(path.atomics).reader)
    //println(FAtomicQuery(HNil).gen(path.atomics))
    //println(FAtomicQuery(needAtomic[JsonReader]).gen(path.atomics).right.get.reader)

    FAtomicQuery(needAtomic[JsonReader] :: needAtomic[JsonReader] :: needAtomic[JsonWriter] :: HNil).gen(path.atomics)
    //println(FAtomicQuery(needAtomic[JsonReader] :: needAtomic[JsonReader] :: needAtomic[JsonWriter] :: needAtomic[JsonWriter] :: HNil).gen(path.atomics))

    val items: List[FAtomic[Long]] = path.atomics
    val Right(reader1 :: reader3 :: reader2 :: writer1 :: writer2 :: writer3 :: writer4 :: HNil) = FAtomicQuery(needAtomic[JsonReader] :: needAtomicOpt[JsonReader] :: needAtomic[JsonReader] :: needAtomic[JsonWriter] :: needAtomic[JsonWriter] :: needAtomic[JsonWriter] :: needAtomic[JsonWriter] :: HNil).gen(items)
    //println(reader2.reader)
    //println(writer1.writer)
    //println(writer4.writer)
    //println(reader3.get.reader)
    //println(FAtomicQuery(needAtomic[JsonReader] :: needAtomic[JsonReader] :: needAtomic[JsonWriter] :: HNil).gen(path.atomics).right.get(2).writer)

    /*val isReaderDefined = (myPath: FPath) => {
      FAtomicQuery(needAtomicOpt[JsonReader] :: needAtomic[JsonWriter] :: HNil).map(myPath) { case readerOpt2 :: writer1 :: HNil =>
        readerOpt2.isDefined
      }
    }*/

    val path1 = FPathImpl(In.jWrite[Long])
    //println(isReaderDefined(path1))
  }

  class FColumnStringImplicits(proName1: String) {
    def column[D](converts: List[FAtomic[D]]): FPathImpl[D] = {
      val proName = new FProperty[D] {
        override val proName = proName1
      }
      FPathImpl(proName :: converts)
    }
    def column[D](converts: FAtomic[D]*): FPathImpl[D] = {
      val proName = new FProperty[D] {
        override val proName = proName1
      }
      FPathImpl(proName :: converts.toList)
    }
    def columns[D](converts: List[FAtomic[D]]*): FPathImpl[D] = {
      val proName = new FProperty[D] {
        override val proName = proName1
      }
      FPathImpl(proName :: converts.toList.flatten)
    }
  }

  implicit def fColumnStringExtesionMethods(proName: String): FColumnStringImplicits = new FColumnStringImplicits(proName)

  "FPile" should "work fine" in {

    trait JsonWriterImpl {
      type DataType
      val key: String
      val encoder: Encoder[DataType]
      val isReaderDefined: Boolean //多余的
      val data: Option[DataType]
    }

    val mainPile = FPile.applyOpt(
      ("我是" columns (In.default(12L) ::: In.jRead[Long] ::: In.jWrite[Long])) ::
      ("小莎莎" columns (In.default("1234") ::: In.jWrite[String])) ::
      (("千反田" columns (In.jRead[Int] ::: In.default(579) ::: In.jWrite[Int])) :: HNil) ::
      (
        ("的枕头" columns (In.jRead[Int] ::: In.jWrite[Int])) ::
        ("喵喵喵" columns (In.jRead[Long] ::: In.default(4564654624463455345L) ::: In.jWrite[Long])) ::
          (
            ("哈哈哈哈哈" columns (In.jRead[Int] ::: In.default(579) ::: In.jWrite[Int])) ::
            ("汪汪汪" columns (In.jRead[Long] ::: In.jWrite[Long])) ::
            HNil
        ) ::
        HNil
      ) ::
      HNil
    )
    val appendPile = FPile.applyOpt(
      ("jilen" columns (In.default("喵") ::: In.jRead[String] ::: In.jWrite[String])) ::
      ("kerr" columns (In.default("汪") ::: In.jRead[String] ::: In.jWrite[String])) ::
      HNil
    )

    val piles: List[FPileAbstract[Option]] = mainPile :: appendPile :: Nil
    val paths = piles.map(s => s.fShape.encodeColumn(s.pathPile)).flatten

    val resultGen1 = FPile.transformOf { path =>
      FAtomicQuery(needAtomicOpt[JsonReader] :: needAtomic[JsonWriter] :: (needAtomicOpt[DefaultValue] :: HNil) :: needAtomic[FProperty] :: HNil)
        .mapToOption(path) { case (readerOpt :: writer :: (defaultOpt :: HNil) :: property :: HNil, data) =>
          val defaultValueOpt = data.fold(defaultOpt.map(_.value))(Option(_))
          //println(property.proName + ":" + defaultValueOpt + "11111111")
          new JsonWriterImpl {
            override type DataType = writer.JsonType
            override val key = property.proName
            override val encoder = writer.writer
            override val isReaderDefined = readerOpt.isDefined
            override val data = defaultValueOpt.map(writer.convert)
          }: JsonWriterImpl
        }
    } { results =>
      results.map { s =>
        implicit val encoderForOpt = s.encoder
        s.key -> s.data.asJson
      }.toMap.asJson
    }

    resultGen1(paths) match {
      case Left(e) => throw e
      case Right(s) =>
        val result = s(piles.map(s => s.fShape.encodeData(s.fShape.zero)).flatten)
        //println(result)
        result
    }

    val resultGen2 = FPile.transformTree { path =>
      FAtomicQuery(needAtomicOpt[JsonReader] :: needAtomic[JsonWriter] :: (needAtomicOpt[DefaultValue] :: HNil) :: needAtomic[FProperty] :: HNil)
        .mapToOption(path) { case (readerOpt :: writer :: (defaultOpt :: HNil) :: property :: HNil, data) =>
          val defaultValueOpt = data.fold(defaultOpt.map(_.value))(Option(_))
          new JsonWriterImpl {
            override type DataType = writer.JsonType
            override val key = property.proName
            override val encoder = writer.writer
            override val isReaderDefined = readerOpt.isDefined
            override val data = defaultValueOpt.map(writer.convert)
          }: JsonWriterImpl
        }
    } { results =>
      results.map { s =>
        implicit val encoderForOpt = s.encoder
        s.key -> s.data.asJson
      }.toMap.asJson
    }

    val jx3Pile = FPile.applyOpt(
      ("小萌师父的徒弟个数" columns (In.default(6L) ::: In.jRead[Long] ::: In.jWrite[Long])) ::
        ("徒弟的名字" columns (In.default("水山清风") ::: In.jWrite[String])) ::
        (("茶馆任务要做多少遍才有奖品" columns (In.default(10) ::: In.jWrite[Int])) :: HNil) ::
        (
          ("狭义值" columns (In.jRead[Int] ::: In.jWrite[Int])) ::
            ("江湖贡献值" columns (In.default(4564654624463455345L) ::: In.jWrite[Long])) ::
            (
              ("大侠之路" columns (In.jRead[String] ::: In.default("跳出五行天地外") ::: In.jWrite[String])) ::
                ("电信" columns (In.default(5L) ::: In.jWrite[Long])) ::
                HNil
            ) ::
            HNil
          ) ::
        HNil
    )

    resultGen2(jx3Pile) match {
      case Left(e) => throw e
      case Right((outPile, s)) =>
        val result = s(jx3Pile.fShape.encodeData(jx3Pile.fShape.zero))
        //println(result)
        result
    }

    val resultGen3 = FPile.transformTreeList { path =>
      FAtomicQuery(needAtomicOpt[JsonReader] :: needAtomic[JsonWriter] :: (needAtomicOpt[DefaultValue] :: HNil) :: needAtomic[FProperty] :: HNil)
        .mapToOption(path) { case (readerOpt :: writer :: (defaultOpt :: HNil) :: property :: HNil, data) =>
          val defaultValueOpt = data.fold(defaultOpt.map(_.value))(Option(_))
          new JsonWriterImpl {
            override type DataType = writer.JsonType
            override val key = property.proName
            override val encoder = writer.writer
            override val isReaderDefined = readerOpt.isDefined
            override val data = defaultValueOpt.map(writer.convert)
          }: JsonWriterImpl
        }
    } { results =>
      results.map { s =>
        implicit val encoderForOpt = s.encoder
        s.key -> s.data.asJson
      }.toMap.asJson
    }

    resultGen3(jx3Pile :: appendPile :: Nil) match {
      case Left(e) => throw e
      case Right((outPile, s)) =>
        val result = s(jx3Pile.fShape.encodeData(jx3Pile.fShape.zero) ::: appendPile.fShape.encodeData(appendPile.fShape.zero))
        //println(result)
        result
    }

    //println("convertPile1:" + convertPile1)

    val resultGen4 = FPile.transformTreeList { path =>
      FAtomicQuery((needAtomicOpt[DefaultValue] :: needAtomic[FProperty] :: HNil) :: HNil)
        .mapToOption(path) { case ((defaultOpt :: property :: HNil) :: HNil, data) =>
          val defaultValueOpt = data.fold(defaultOpt.map(_.value))(Option(_))
          //println(defaultValueOpt)
          //println(property.proName)
          defaultValueOpt: Option[Any]
        }
    } { result =>
      //println(result)
      result
    }

    val resultGen5 = FPile.transformTreeList { path =>
      FAtomicQuery(needAtomic[JsonWriter] :: (needAtomicOpt[DefaultValue] :: HNil) :: needAtomic[FProperty] :: HNil)
        .mapToOption(path) { case (writer :: (defaultOpt :: HNil) :: property :: HNil, data1) =>
          //println(data1)
          val defaultValueOpt = data1.fold(defaultOpt.map(_.value))(Option(_))
          new JsonWriterImpl {
            override type DataType = writer.JsonType
            override val key = property.proName
            override val encoder = writer.writer
            override val isReaderDefined = defaultOpt.isDefined
            override val data = defaultValueOpt.map(writer.convert)
          }: JsonWriterImpl
        }
    } { results =>
      results.map { s =>
        implicit val encoderForOpt = s.encoder
        s.key -> s.data.asJson
      }.toMap.asJson
    }

    val mainPile1 = FPile.applyOpt(
      ("我是" columns (In.default(12L) ::: In.jRead[Long] ::: In.jWrite[Long])) ::
      ("小莎莎" columns (In.default("1234") ::: In.jWrite[String])) ::
      HNil
    )
    val appendPile1 = FPile.applyOpt(
      ("jilen" columns (In.default("喵") ::: In.jRead[String] ::: In.jWrite[String])) ::
      ("kerr" columns (In.default("汪") ::: In.jRead[String])) ::
      HNil
    )

    val convertPile1 = (mainPile1 :: appendPile1 :: HNil).poly(FPile.applyOpt(
      ("小萌师父" columns (In.default("喵") ::: In.jRead[String] ::: In.jWrite[String])) ::
      ("徒弟弟" columns (In.default(6L) ::: In.jRead[Long] ::: In.jWrite[Long])) ::
      HNil
    )) { case (longData :: stringData :: HNil) :: (stringData2 :: stringData3 :: HNil) :: HNil =>
      None :: None :: HNil
    }

    val pileList = convertPile1 :: mainPile1 :: Nil
    println(convertPile1.toString)

    try {
      resultGen4(pileList) match {
        case Left(e) => throw e
        case Right((outPile, s)) =>
          val result = s(pileList.flatMap(_.deepZero))
          resultGen5(outPile) match {
            case Left(e1) => throw e1
            case Right((outPile1, s1)) =>
              //println(outPile1)
              s1(result)
              println(s1(result))
          }
      }
    } catch {
      case e: Exception => e.printStackTrace
    }

  }

}