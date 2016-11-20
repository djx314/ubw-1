package net.scalax.fsn.database.test

import net.scalax.fsn.core.{AbstractFPathGen, FPathGen, FPathGenImpl, FPathImpl}
import org.scalatest._
import org.scalatest.concurrent.ScalaFutures
import net.scalax.fsn.json.atomic.JsonReader
import net.scalax.fsn.mix.helpers.In

class ParTest extends FlatSpec
  with Matchers
  with EitherValues
  with ScalaFutures
  with BeforeAndAfterAll
  with BeforeAndAfter
  with FPathGenImpl {

  "shapes" should "find readers in Atomic in FPath" in {
    val path = FPathImpl(In.jRead[Long] ::: In.jWrite[Long])
    val bb: FPathGen[JsonReader] = needAtomic[JsonReader]
    val jsonReaderGen: AbstractFPathGen = needAtomic[JsonReader]
    println(jsonReaderGen.gen(path))
  }

}