package net.scalax.fsn.database.test

import net.scalax.fsn.core._
import org.scalatest._
import org.scalatest.concurrent.ScalaFutures
import net.scalax.fsn.json.atomic.{JsonReader, JsonWriter}
import net.scalax.fsn.mix.helpers.In
import shapeless._

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

}