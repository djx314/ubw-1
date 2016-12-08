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
  with FAtomicGenImpl
  with FAtomicShapeHelper {

  "shapes" should "find readers in Atomic in FPath" in {
    val path = FPathImpl(In.jRead[Long] ::: In.jWrite[Long])
    val bb: FAtomicGen[JsonReader] = needAtomic[JsonReader]
    val jsonReaderGen: AbstractFAtomicGen = needAtomic[JsonReader]

    println(bb.gen(path.atomics).reader)
    println(FAtomicQuery(HNil).gen(path.atomics))
    println(FAtomicQuery(needAtomic[JsonReader]).gen(path.atomics).right.get.reader)

    FAtomicQuery(needAtomic[JsonReader] :: needAtomic[JsonReader] :: needAtomic[JsonWriter] :: HNil).gen(path.atomics)
    println(FAtomicQuery(needAtomic[JsonReader] :: needAtomic[JsonReader] :: needAtomic[JsonWriter] :: needAtomic[JsonWriter] :: HNil).gen(path.atomics))

    val items: List[FAtomic[Long]] = path.atomics
    val Right(reader1 :: reader3 :: reader2 :: writer1 :: writer2 :: writer3 :: writer4 :: HNil) = FAtomicQuery(needAtomic[JsonReader] :: needAtomicOpt[JsonReader] :: needAtomic[JsonReader] :: needAtomic[JsonWriter] :: needAtomic[JsonWriter] :: needAtomic[JsonWriter] :: needAtomic[JsonWriter] :: HNil).gen(items)
    println(reader2.reader)
    println(writer1.writer)
    println(writer4.writer)
    println(reader3.get.reader)
    println(FAtomicQuery(needAtomic[JsonReader] :: needAtomic[JsonReader] :: needAtomic[JsonWriter] :: HNil).gen(path.atomics).right.get(2).writer)

    val isReaderDefined = (myPath: FPath) => {
      FAtomicQuery(needAtomicOpt[JsonReader] :: needAtomic[JsonWriter] :: HNil).map(myPath) { case readerOpt2 :: writer1 :: HNil =>
        readerOpt2.isDefined
      }
    }

    val path1 = FPathImpl(In.jWrite[Long])
    println(isReaderDefined(path1))
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
      val data: DataType
    }

    val pile = FPile.applyOpt(
      FPathImpl(In.property("我是") :: In.default(12L) ::: In.jRead[Long] ::: In.jWrite[Long]) ::
      ("小莎莎" columns (In.default("1234") ::: In.jWrite[String])) ::
      ("的枕头" columns (In.jRead[Int] ::: In.default(579) ::: In.jWrite[Int])) ::
      HNil
    )
    val paths = pile.fShape.encodeColumn(pile.pathPile)

    val resultGen = FPile.transform { path =>
      FAtomicQuery(needAtomicOpt[JsonReader] :: needAtomic[JsonWriter] :: needAtomic[DefaultValue] :: needAtomic[FProperty] :: HNil)
      .map(path) { case readerOpt :: writer :: default :: property :: HNil =>
        new JsonWriterImpl {
          override type DataType = writer.JsonType
          override val key = property.proName
          override val encoder = writer.writer
          override val isReaderDefined = readerOpt.isDefined
          override val data = writer.convert(default.value)
        }: JsonWriterImpl
      }
    } { results =>
      results.map { s =>
        s.key -> s.data.asJson(s.encoder)
      }.toMap.asJson
    }

    resultGen(paths) match {
      case Left(e) => throw e
      case Right(s) =>
        println(s)
        s
    }

  }

}