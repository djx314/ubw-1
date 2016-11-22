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
  with FAtomicShapeImpl {

  "shapes" should "find readers in Atomic in FPath" in {
    /*val path = FPathImpl(In.jRead[Long] ::: In.jWrite[Long])
    val bb: FAtomicGen[JsonReader] = needAtomic[JsonReader]
    val jsonReaderGen: AbstractFAtomicGen = needAtomic[JsonReader]

    println(bb.gen(path.atomics).reader)
    println(FAtomicQuery(HNil).gen(path.atomics))
    println(FAtomicQuery(needAtomic[JsonReader]).gen(path.atomics).reader)

    FAtomicQuery(needAtomic[JsonReader] :: needAtomic[JsonReader] :: needAtomic[JsonWriter] :: HNil).gen(path.atomics)
    println(FAtomicQuery(needAtomic[JsonReader] :: needAtomic[JsonReader] :: needAtomic[JsonWriter] :: needAtomic[JsonWriter] :: HNil).gen(path.atomics))*/

    val path = FPathImpl(In.jRead[Long] ::: In.jWrite[Long])
    val items: List[FAtomic[Long]] = path.atomics
    val reader1 :: reader2 :: writer1 :: HNil = FAtomicQuery(needAtomic[JsonReader] :: needAtomic[JsonReader] :: needAtomic[JsonWriter] :: HNil).gen(path.atomics)
    case class Ab[S, T](s: S, t: T)
    val s Ab t = Ab(1, "2")
    println(s)
    println(t)
    val a Ab b Ab c = Ab(Ab(1, "2"), 3L)
    println(a)
    println(b)
    println(c)
  }

}