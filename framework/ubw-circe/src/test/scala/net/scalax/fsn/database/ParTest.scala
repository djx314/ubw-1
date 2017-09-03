package net.scalax.fsn.database.test

import net.scalax.fsn.core._
import org.scalatest._
import org.scalatest.concurrent.ScalaFutures
import net.scalax.fsn.json.atomic.{ JsonReader, JsonWriter }
import net.scalax.fsn.mix.helpers.SlickCRUDImplicits
import shapeless._
import io.circe._
import io.circe.syntax._
import net.scalax.fsn.common.atomic.{ DefaultValue, FProperty }
import net.scalax.fsn.json.operation.{ AtomicValueHelper, FDefaultAtomicHelper, FPropertyAtomicHelper }
import net.scalax.fsn.slick.helpers.{ FJsonAtomicHelper, FStrSelectExtAtomicHelper }
import scala.language.implicitConversions

class ParTest extends FlatSpec
    with Matchers
    with EitherValues
    with ScalaFutures
    with BeforeAndAfterAll
    with BeforeAndAfter
    with PilesPolyHelper
    with PilesGenHelper
    with SlickCRUDImplicits
    with AtomicValueHelper {

  implicit def fPilesOptionImplicit[D](path: AtomicPathImpl[D]): FJsonAtomicHelper[D] with FStrSelectExtAtomicHelper[D] with FPropertyAtomicHelper[D] with FDefaultAtomicHelper[D] = {
    val path1 = path
    new FJsonAtomicHelper[D] with FStrSelectExtAtomicHelper[D] with FPropertyAtomicHelper[D] with FDefaultAtomicHelper[D] {
      override val path = path1
    }
  }

  "shapes" should "find readers in Atomic in FPath" in {
    val path = emptyPath[Long].readJ.writeJ

    new AtomicQuery(path) {
      val aa = withRep(needAtomic[JsonReader] :: needAtomic[JsonReader] :: needAtomic[JsonWriter] :: FANil)
    }

    val Right(reader1 :: reader3 :: reader2 :: writer1 :: writer2 :: writer3 :: writer4 :: HNil) = new AtomicQuery(path) {
      val aa = withRep(needAtomic[JsonReader] :: needAtomicOpt[JsonReader] :: needAtomic[JsonReader] :: needAtomic[JsonWriter] :: needAtomic[JsonWriter] :: needAtomic[JsonWriter] :: needAtomic[JsonWriter] :: FANil)
    }.aa.queryResult

    val path1 = emptyPath[Long].writeJ
  }

  "Pile" should "work fine" in {

    trait JsonWriterImpl {
      type DataType
      val key: String
      val encoder: Encoder[DataType]
      val isReaderDefined: Boolean //多余的
      val data: Option[DataType]
    }

    val resultGen1 = Pile.transformOf {
      new AtomicQuery(_) {
        val aa = withRep(needAtomicOpt[JsonReader] :: needAtomic[JsonWriter] :: (needAtomicOpt[DefaultValue] :: FANil) :: needAtomic[FProperty] :: FANil)
          .mapTo {
            case (readerOpt :: writer :: (defaultOpt :: HNil) :: property :: HNil, data) =>
              val defaultValueOpt = mergeDefault(defaultOpt, data)
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

    val resultGen3 = Pile.transformTreeList {
      new AtomicQuery(_) {
        val aa = withRep(needAtomicOpt[JsonReader] :: needAtomic[JsonWriter] :: (needAtomicOpt[DefaultValue] :: FANil) :: needAtomic[FProperty] :: FANil)
          .mapTo {
            case (readerOpt :: writer :: (defaultOpt :: HNil) :: property :: HNil, data) =>
              val defaultValueOpt = mergeDefault(defaultOpt, data)
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

    val resultGen4 = Pile.transformTreeList {
      new AtomicQuery(_) {
        val aa = withRep((needAtomicOpt[DefaultValue] :: needAtomic[FProperty] :: FANil) :: FANil)
          .mapTo {
            case ((defaultOpt :: property :: HNil) :: HNil, data) =>
              val defaultValueOpt = mergeDefault(defaultOpt, data)
              defaultValueOpt: Option[Any]
          }
      }.aa
    } { result =>
      //println(result)
      result
    }

    val resultGen5 = Pile.transformTreeList {
      new AtomicQuery(_) {
        val aa = withRep(needAtomic[JsonWriter] :: (needAtomicOpt[DefaultValue] :: FANil) :: needAtomic[FProperty] :: FANil)
          .mapTo {
            case (writer :: (defaultOpt :: HNil) :: property :: HNil, data1) =>
              val defaultValueOpt = mergeDefault(defaultOpt, data1)
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
      ("我是" ofPile emptyPath[Long].defaultValue(12345678L).readJ.writeJ) ::
        ("小莎莎" ofPile emptyPath[String].defaultValue("1234").writeJ) ::
        FPNil

    val appendPile1 =
      ("jilen" ofPile emptyPath[String].defaultValue("喵").readJ.writeJ) ::
        ("kerr" ofPile emptyPath[String].defaultValue("汪").readJ) ::
        FPNil

    val convertPile1 = (mainPile1 :: appendPile1 :: FPNil).poly(
      ("小萌师父" ofPile emptyPath[String].defaultValue("喵").readJ) ::
        ("徒弟弟" ofPile emptyPath[Long].defaultValue(6L).readJ.writeJ) ::
        FPNil
    ).transform {
        case (longData :: stringData :: HNil) :: (stringData2 :: stringData3 :: HNil) :: HNil =>
          emptyValue[String] :: emptyValue[Long] :: HNil
      }

    val convertPile2 = (convertPile1 :: mainPile1 :: FPNil).poly(
      ("喵喵喵" ofPile emptyPath[String].defaultValue("喵").readJ.writeJ) ::
        ("汪汪汪" ofPile emptyPath[Long].defaultValue(5678L).readJ.writeJ) ::
        FPNil
    ).transform {
        case (stringData :: longData1 :: HNil) :: (longData2 :: stringData3 :: HNil) :: HNil =>
          emptyValue[String] :: longData2 :: HNil
      }

  }

}