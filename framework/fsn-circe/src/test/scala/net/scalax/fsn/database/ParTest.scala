package net.scalax.fsn.database.test

import net.scalax.fsn.core._
import org.scalatest._
import org.scalatest.concurrent.ScalaFutures
import net.scalax.fsn.json.atomic.{ JsonReader, JsonWriter }
import net.scalax.fsn.mix.helpers.{ In, SlickCRUDImplicits }
import shapeless._
import io.circe._
import io.circe.syntax._
import net.scalax.fsn.common.atomic.{ DefaultValue, FProperty }
import net.scalax.fsn.json.operation.FAtomicValueHelper

class ParTest extends FlatSpec
    with Matchers
    with EitherValues
    with ScalaFutures
    with BeforeAndAfterAll
    with BeforeAndAfter
    with PilesPolyHelper
    with FPilesGenHelper
    with SlickCRUDImplicits
    with FAtomicValueHelper {

  "shapes" should "find readers in Atomic in FPath" in {
    val path = FAtomicPathImpl(In.jRead[Long] ::: In.jWrite[Long])

    new FAtomicQuery(path) {
      val aa = withRep(needAtomic[JsonReader] :: needAtomic[JsonReader] :: needAtomic[JsonWriter] :: FANil)
    }

    val Right(reader1 :: reader3 :: reader2 :: writer1 :: writer2 :: writer3 :: writer4 :: HNil) = new FAtomicQuery(path) {
      val aa = withRep(needAtomic[JsonReader] :: needAtomicOpt[JsonReader] :: needAtomic[JsonReader] :: needAtomic[JsonWriter] :: needAtomic[JsonWriter] :: needAtomic[JsonWriter] :: needAtomic[JsonWriter] :: FANil)
    }.aa.queryResult
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

    val path1 = FAtomicPathImpl(In.jWrite[Long])
    //println(isReaderDefined(path1))
  }

  "FPile" should "work fine" in {

    trait JsonWriterImpl {
      type DataType
      val key: String
      val encoder: Encoder[DataType]
      val isReaderDefined: Boolean //多余的
      val data: Option[DataType]
    }

    val mainPile = (
      ("我是" ofPile FAtomicPathImpl(In.default(12L) ::: In.jRead[Long] ::: In.jWrite[Long])) ::
      ("小莎莎" ofPile FAtomicPathImpl(In.default("1234") ::: In.jWrite[String])) ::
      (("千反田" ofPile FAtomicPathImpl(In.jRead[Int] ::: In.default(579) ::: In.jWrite[Int])) :: HNil) ::
      (
        ("的枕头" ofPile FAtomicPathImpl(In.jRead[Int] ::: In.jWrite[Int])) ::
        ("喵喵喵" ofPile FAtomicPathImpl(In.jRead[Long] ::: In.default(4564654624463455345L) ::: In.jWrite[Long])) ::
        (
          ("哈哈哈哈哈" ofPile FAtomicPathImpl(In.jRead[Int] ::: In.default(579) ::: In.jWrite[Int])) ::
          ("汪汪汪" ofPile FAtomicPathImpl(In.jRead[Long] ::: In.jWrite[Long])) ::
          HNil
        ) ::
          HNil
      ) ::
          HNil
    )
    val appendPile = (
      ("jilen" ofPile FAtomicPathImpl(In.default("喵") ::: In.jRead[String] ::: In.jWrite[String])) ::
      ("kerr" ofPile FAtomicPathImpl(In.default("汪") ::: In.jRead[String] ::: In.jWrite[String])) ::
      HNil
    )

    //val piles: List[FPileAbstract[Option]] = mainPile :: appendPile :: Nil
    //val paths = piles.map(s => s.fShape.encodeColumn(s.pathPile)).flatten

    val resultGen1 = FPile.transformOf {
      new FAtomicQuery(_) {
        val aa = withRep(needAtomicOpt[JsonReader] :: needAtomic[JsonWriter] :: (needAtomicOpt[DefaultValue] :: FANil) :: needAtomic[FProperty] :: FANil)
          .mapTo {
            case (readerOpt :: writer :: (defaultOpt :: HNil) :: property :: HNil, data) =>
              val defaultValueOpt = mergeDefault(defaultOpt, data) //data.opt.fold(defaultOpt.map(_.value))(Option(_))
              //println(property.proName + ":" + defaultValueOpt + "11111111")
              new JsonWriterImpl {
                override type DataType = path.DataType
                override val key = property.proName
                override val encoder = writer.writer
                override val isReaderDefined = readerOpt.isDefined
                override val data = defaultValueOpt
              }: JsonWriterImpl
          }
      }.aa
    } { results =>
      results.map { s =>
        implicit val encoderForOpt = s.encoder
        s.key -> s.data.asJson
      }.toMap.asJson
    }

    /*resultGen1(paths) match {
      case Left(e) => throw e
      case Right(s) =>
        val result = s(piles.map(s => s.fShape.encodeData(s.fShape.zero)).flatten)
        //println(result)
        result
    }*/

    /*val resultGen2 = FPile.transformTree {
      new FAtomicQuery(_) {
        val aa = withRep(needAtomicOpt[JsonReader] :: needAtomic[JsonWriter] :: (needAtomicOpt[DefaultValue] :: FANil) :: needAtomic[FProperty] :: FANil)
          .mapTo {
            case (readerOpt :: writer :: (defaultOpt :: HNil) :: property :: HNil, data) =>
              val defaultValueOpt = mergeDefault(defaultOpt, data) //data.opt.fold(defaultOpt.map(_.value))(Option(_))
              new JsonWriterImpl {
                override type DataType = path.DataType
                override val key = property.proName
                override val encoder = writer.writer
                override val isReaderDefined = readerOpt.isDefined
                override val data = defaultValueOpt
              }: JsonWriterImpl
          }
      }.aa
    } { results =>
      results.map { s =>
        implicit val encoderForOpt = s.encoder
        s.key -> s.data.asJson
      }.toMap.asJson
    }*/

    val jx3Pile = (
      ("小萌师父的徒弟个数" ofPile FAtomicPathImpl(In.default(6L) ::: In.jRead[Long] ::: In.jWrite[Long])) ::
      ("徒弟的名字" ofPile FAtomicPathImpl(In.default("水山清风") ::: In.jWrite[String])) ::
      (("茶馆任务要做多少遍才有奖品" ofPile FAtomicPathImpl(In.default(10) ::: In.jWrite[Int])) :: HNil) ::
      (
        ("狭义值" ofPile FAtomicPathImpl(In.jRead[Int] ::: In.jWrite[Int])) ::
        ("江湖贡献值" ofPile FAtomicPathImpl(In.default(4564654624463455345L) ::: In.jWrite[Long])) ::
        (
          ("大侠之路" ofPile FAtomicPathImpl(In.jRead[String] ::: In.default("跳出五行天地外") ::: In.jWrite[String])) ::
          ("电信" ofPile FAtomicPathImpl(In.default(5L) ::: In.jWrite[Long])) ::
          HNil
        ) ::
          HNil
      ) ::
          HNil
    )

    /*resultGen2(jx3Pile) match {
      case Left(e) => throw e
      case Right((outPile, s)) =>
        val result = s(jx3Pile.fShape.encodeData(jx3Pile.fShape.zero))
        result
    }*/

    val resultGen3 = FPile.transformTreeList {
      new FAtomicQuery(_) {
        val aa = withRep(needAtomicOpt[JsonReader] :: needAtomic[JsonWriter] :: (needAtomicOpt[DefaultValue] :: FANil) :: needAtomic[FProperty] :: FANil)
          .mapTo {
            case (readerOpt :: writer :: (defaultOpt :: HNil) :: property :: HNil, data) =>
              val defaultValueOpt = mergeDefault(defaultOpt, data) //data.opt.fold(defaultOpt.map(_.value))(Option(_))
              new JsonWriterImpl {
                override type DataType = path.DataType
                override val key = property.proName
                override val encoder = writer.writer
                override val isReaderDefined = readerOpt.isDefined
                override val data = defaultValueOpt
              }: JsonWriterImpl
          }
      }.aa
    } { results =>
      results.map { s =>
        implicit val encoderForOpt = s.encoder
        s.key -> s.data.asJson
      }.toMap.asJson
    }

    /*resultGen3(jx3Pile :: appendPile :: Nil) match {
      case Left(e) => throw e
      case Right((outPile, s)) =>
        val result = s(jx3Pile.fShape.encodeData(jx3Pile.fShape.zero) ::: appendPile.fShape.encodeData(appendPile.fShape.zero))
        //println(result)
        result
    }*/

    //println("convertPile1:" + convertPile1)

    val resultGen4 = FPile.transformTreeList {
      new FAtomicQuery(_) {
        val aa = withRep((needAtomicOpt[DefaultValue] :: needAtomic[FProperty] :: FANil) :: FANil)
          .mapTo {
            case ((defaultOpt :: property :: HNil) :: HNil, data) =>
              val defaultValueOpt = mergeDefault(defaultOpt, data) //data.opt.fold(defaultOpt.map(_.value))(Option(_))
              //println(defaultValueOpt)
              //println(property.proName)
              defaultValueOpt: Option[Any]
          }
      }.aa
    } { result =>
      //println(result)
      result
    }

    val resultGen5 = FPile.transformTreeList {
      new FAtomicQuery(_) {
        val aa = withRep(needAtomic[JsonWriter] :: (needAtomicOpt[DefaultValue] :: FANil) :: needAtomic[FProperty] :: FANil)
          .mapTo {
            case (writer :: (defaultOpt :: HNil) :: property :: HNil, data1) =>
              //println(data1)
              val defaultValueOpt = mergeDefault(defaultOpt, data1) //data1.opt.fold(defaultOpt.map(_.value))(Option(_))
              new JsonWriterImpl {
                override type DataType = path.DataType
                override val key = property.proName
                override val encoder = writer.writer
                override val isReaderDefined = defaultOpt.isDefined
                override val data = defaultValueOpt
              }: JsonWriterImpl
          }
      }.aa
    } { results =>
      results.map { s =>
        implicit val encoderForOpt = s.encoder
        s.key -> s.data.asJson
      }.toMap.asJson
    }

    val mainPile1 =
      ("我是" ofPile FAtomicPathImpl(In.default(12345678L) ::: In.jRead[Long] ::: In.jWrite[Long])) ::
        ("小莎莎" ofPile FAtomicPathImpl(In.default("1234") ::: In.jWrite[String])) ::
        FPNil

    val appendPile1 =
      ("jilen" ofPile FAtomicPathImpl(In.default("喵") ::: In.jRead[String] ::: In.jWrite[String])) ::
        ("kerr" ofPile FAtomicPathImpl(In.default("汪") ::: In.jRead[String])) ::
        FPNil

    val convertPile1 = (mainPile1 :: appendPile1 :: FPNil).poly(
      ("小萌师父" ofPile FAtomicPathImpl(In.default("喵") ::: In.jRead[String])) ::
        ("徒弟弟" ofPile FAtomicPathImpl(In.default(6L) ::: In.jRead[Long] ::: In.jWrite[Long])) ::
        FPNil
    ).transform {
        case (longData :: stringData :: HNil) :: (stringData2 :: stringData3 :: HNil) :: HNil =>
          emptyValue[String] :: emptyValue[Long] :: HNil
      }

    val convertPile2 = (convertPile1 :: mainPile1 :: FPNil).poly(
      ("喵喵喵" ofPile FAtomicPathImpl(In.default("喵") ::: In.jRead[String] ::: In.jWrite[String])) ::
        ("汪汪汪" ofPile FAtomicPathImpl(In.default(5678L) ::: In.jRead[Long] ::: In.jWrite[Long])) ::
        FPNil
    ).transform {
        case (stringData :: longData1 :: HNil) :: (longData2 :: stringData3 :: HNil) :: HNil =>
          emptyValue[String] :: longData2 :: HNil
      }

    /*val pileList = convertPile2 :: mainPile1 :: Nil
    println(convertPile1.toString)

    println(resultGen4.flatMap(resultGen5) {
      case (result, gen) =>
        gen(result)
    }.result(pileList))*/

    /*try {
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
    }*/

  }

}